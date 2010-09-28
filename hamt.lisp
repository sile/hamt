(in-package :hamt)

(declaim (inline make-key/value entry-count))

(defstruct (key/value (:conc-name k/v-)
                      (:constructor make-key/value (key value)))
  key
  value)

;; XXX:
(defvar *empty-vec* #())

(defstruct hamt
  (root-entries #()   :type simple-vector)
  (new-root-entries *empty-vec* :type simple-vector)
  (root-bitlen 0      :type fixnum-length)
  (test #'equal       :type function)       
  (hash #'hash      :type function)
  (resize-threshold 0 :type positive-fixnum)
  (entry-count 0      :type positive-fixnum))

(defun make (&key (test #'equal) (hash #'hash) (size 16))
  (declare (function test hash))
  (let ((bit-length (ceiling (log size 2))))
    (make-hamt :root-entries (make-array (ash 2 (1- bit-length)) :initial-element nil)
               :root-bitlen bit-length
               :resize-threshold (ash 2 (the fixnum-length (+ -1 bit-length +PER-ARC-BIT-LENGTH+)))
               :test test :hash hash)))

(defun entry-count (hamt)
  (declare #.*interface* (hamt hamt))
  (hamt-entry-count hamt))

(defun get (key hamt)
  (declare #.*interface* (hamt hamt))
  (let ((root-entries (hamt-root-entries hamt))
        (new-root-entries (hamt-new-root-entries hamt))
        (root-bitlen  (hamt-root-bitlen hamt))
        (resize-threshold (hamt-resize-threshold hamt))
        (hash         (hamt-hash hamt)))
    (declare #.*fastest*)
    (let ((in (new-arc-stream key :hash hash)))
      (declare (dynamic-extent in))
      (loop WITH node = nil
            FOR arc OF-TYPE positive-fixnum  = (read-n-arc in root-bitlen) THEN (read-arc in)
            FOR entry = (aref (if (< arc resize-threshold)
                                  root-entries (progn (incf arc (the positive-fixnum (ash (read-arc in) root-bitlen))) new-root-entries))
                              arc) THEN (get-entry node arc)
        DO
        (typecase entry
          (null     (return (values nil nil)))
          (amt-node (setf node entry))        
          (key/value     
           (return (if (funcall (hamt-test hamt) key (k/v-key entry))  
                       (values (k/v-value entry) t)
                     (values nil nil)))))))))

(defun resolve-collision (kv1 kv2 arc-start rehash-count node hamt)
  (declare #.*fastest*
           (key/value kv1 kv2)
           (arc-start arc-start)
           (positive-fixnum rehash-count))
  (with-slots (test hash) hamt
    (with-fields (key value) kv1
      (with-fields (key value) kv2
        (let ((in1 (new-arc-stream kv1.key :hash hash
                                           :start arc-start 
                                           :rehash-count rehash-count))
              (in2 (new-arc-stream kv2.key :hash hash
                                           :start arc-start 
                                           :rehash-count rehash-count)))
          (declare (dynamic-extent in1 in2))
          (loop FOR a1 = (read-arc in1)
                FOR a2 = (read-arc in2)
                WHILE (= a1 a2) 
            DO 
            (setf node (set-entry node a1 (make-amt-node)))
            
            FINALLY
            (set-entry node a1 kv1)
            (set-entry node a2 kv2)))))))

(defun resize-one (hamt)
  (declare (hamt hamt))
  (with-slots (root-entries hash root-bitlen new-root-entries resize-threshold) hamt
    (cond ((= resize-threshold #1=(length root-entries)) ; TODO(?): eq new old
           (setf new-root-entries (make-array (ash #1# +PER-ARC-BIT-LENGTH+) :initial-element nil)))
          ((< resize-threshold #1#)
           (let ((e (aref root-entries resize-threshold))
                 (base resize-threshold))
             (typecase e
               (amt-node (loop FOR arc FROM 0 BELOW +BITMAP-SIZE+ ;; TODO: each-entry
                               FOR sub-e = (get-entry e arc)
                               WHEN sub-e
                           DO
                           (setf (aref new-root-entries (+ (the positive-fixnum (ash arc root-bitlen)) base)) sub-e))
                         (free-entries (amt-node-entries e) *entries-pool*))

               (key/value (let ((in (new-arc-stream (k/v-key e) :hash hash)))
                            (declare (dynamic-extent in))
                            (setf (aref new-root-entries (read-n-arc in (+ root-bitlen +PER-ARC-BIT-LENGTH+))) e)))))
           (setf (aref root-entries resize-threshold) nil)
           (when (zerop resize-threshold)
             (setf root-entries new-root-entries
                   resize-threshold (ash (length new-root-entries) +PER-ARC-BIT-LENGTH+))
             (incf root-bitlen +PER-ARC-BIT-LENGTH+)))
          )))

(defun set-impl (key value hamt)
  (declare #.*interface* (hamt hamt))
  (with-slots (root-entries new-root-entries root-bitlen test hash entry-count resize-threshold) hamt
    (declare #.*fastest*)
    (let ((in (new-arc-stream key :hash hash)))
      (declare (dynamic-extent in))
      (loop WITH node = nil
            FOR arc OF-TYPE positive-fixnum  = (read-n-arc in root-bitlen) THEN (read-arc in)
            FOR entry = (aref (if (< arc resize-threshold)
                                  root-entries (progn (incf arc (the positive-fixnum (ash (read-arc in) root-bitlen))) new-root-entries))
                              arc) THEN (get-entry node arc)
        DO
        (typecase entry
          (null (incf entry-count)
                (return (prog1 (if node
                                   (set-entry node arc (make-key/value key value))
                                 (setf (aref #1=(if (< arc resize-threshold) root-entries new-root-entries)
                                             arc) (make-key/value key value)))
                          (decf resize-threshold)
                          (resize-one hamt))))
          (amt-node  (setf node entry))
          (key/value 
           (return (if (funcall test key (k/v-key entry))
                       (setf (k/v-value entry) value)
                     (progn
                       (incf entry-count)
                       (resolve-collision (make-key/value key value) entry
                                          (arc-stream-start in) (arc-stream-rehash-count in)
                                          (if node 
                                              (set-entry node arc (make-amt-node))
                                            (setf (aref #1# arc) (make-amt-node)))
                                          hamt)
                       (decf resize-threshold)
                       (resize-one hamt))))))))))
                       

(defsetf get (key hamt) (new-value)
  `(progn (set-impl ,key ,new-value ,hamt)  
          ,new-value))