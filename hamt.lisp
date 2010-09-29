(in-package :hamt)

(declaim (inline entry-count read-root-arc))

(defstruct hamt
  (root-entries #()                :type simple-vector)
  (new-root-entries +EMPTY-VECTOR+ :type simple-vector)
  (root-bitlen 0                   :type fixnum-length)
  (test #'equal                    :type function)       
  (hash #'hash                     :type function)
  (resize-border 0                 :type positive-fixnum)
  (entry-count 0                   :type positive-fixnum))

(defun make (&key (test #'equal) (hash #'default-hash) (size 8))
  (declare #.*interface*
           #.*muffle-warning*
           (function test hash)
           (positive-fixnum size))
  (let* ((bit-length (ceiling (log size 2)))
         (init-size  (fixash 2 (1- bit-length))))
    (declare (fixnum-length bit-length))
    (make-hamt :root-entries (make-array init-size :initial-element nil)
               :root-bitlen bit-length
               :resize-border (fixash init-size +PER-ARC-BIT-LENGTH+)
               :test test :hash hash)))

(defun entry-count (hamt)
  (declare #.*interface* (hamt hamt))
  (hamt-entry-count hamt))

(defun read-root-arc (in hamt)
  (declare (hamt hamt)
           (arc-stream in))
  (with-slots (resize-border root-bitlen root-entries new-root-entries) hamt
    (let ((arc (read-n-arc in root-bitlen)))
      (if (< arc resize-border)
          (values arc root-entries)
        (let ((new-arc (the positive-fixnum (+ arc (fixash (read-arc in) root-bitlen)))))
          (values new-arc new-root-entries))))))

(defmacro find-entry-case ((entry entry-place) (in hamt) &key on-succeeded on-failed) 
  (let ((arc (gensym))
        (entries (gensym))
        (node (gensym)))
  `(multiple-value-bind (,arc ,entries) (read-root-arc ,in ,hamt)
     (let ((,entry (aref ,entries ,arc)))
       (symbol-macrolet ((,entry-place (aref ,entries ,arc)))
         (typecase ,entry
           (null      ,on-failed)
           (key/value ,on-succeeded)
           (otherwise ;; In the case, the entry type is always amt-node
            (let ((,node ,entry))
              (declare (amt-node ,node))
              (loop FOR ,arc = (read-arc ,in)
                    FOR ,entry = (get-entry ,node ,arc)
                DO
                (symbol-macrolet ((,entry-place (get-entry ,node ,arc)))
                  (typecase ,entry
                    (null      (return ,on-failed))
                    (key/value (return ,on-succeeded))
                    (otherwise ;; In the case, the entry type is always amt-node
                     (setf ,node ,entry)))))))))))))

(defun get (key hamt)
  (declare #.*interface* (hamt hamt))
  (with-slots (hash test) hamt
    (declare #.*fastest*)
    (let ((in (new-arc-stream key :hash hash)))
      (declare (dynamic-extent in))
      (find-entry-case (entry entry-place) (in hamt) 
        :on-failed (values nil nil)
        :on-succeeded 
          (if (funcall test key (k/v-key entry))
              (values (k/v-value entry) t)
            (values nil nil))))))

(defun resolve-collision (kv1 kv2 arc-start rehash-count node hamt)
  (declare #.*fastest*
           (key/value kv1 kv2)
           (arc-start arc-start)
           (positive-fixnum rehash-count))
  (with-fields (key value) kv1
    (with-fields (key value) kv2
      (let ((in1 (new-arc-stream kv1.key :hash (hamt-hash hamt)
                                         :start arc-start 
                                         :rehash-count rehash-count))
            (in2 (new-arc-stream kv2.key :hash (hamt-hash hamt)
                                         :start arc-start 
                                         :rehash-count rehash-count)))
        (declare (dynamic-extent in1 in2))
        (loop FOR a1 = (read-arc in1)
              FOR a2 = (read-arc in2)
              WHILE (= a1 a2) 
          DO 
          (setf node (setf (get-entry node a1) (make-amt-node)))
          
          FINALLY
          (set-new-2-entries node a1 kv1 a2 kv2))))))

(defun amortized-resize (hamt)
  (declare #.*fastest*
           (hamt hamt))
  (with-slots (root-entries hash root-bitlen new-root-entries resize-border) hamt
    (decf resize-border)
    (let ((cur-size (length root-entries)))
      (cond ((= resize-border cur-size)
             (setf new-root-entries 
                   (make-array (fixash cur-size +PER-ARC-BIT-LENGTH+) :initial-element nil)))
            ((< resize-border cur-size)
             (let* ((root-arc resize-border)
                    (entry (aref root-entries root-arc))
                    (new-root-bitlen (+ root-bitlen +PER-ARC-BIT-LENGTH+)))
               (declare (fixnum-length new-root-bitlen))
               (typecase entry
                 (amt-node (each-entry (arc sub-entry) entry
                             (let ((adjusted-arc (+ (fixash arc root-bitlen) root-arc)))
                               (setf (aref new-root-entries adjusted-arc) sub-entry)))
                           (free-entries (amt-node-entries entry)))
                 (key/value (let ((in (new-arc-stream (k/v-key entry) :hash hash)))
                              (declare (dynamic-extent in))
                              (setf (aref new-root-entries (read-n-arc in new-root-bitlen)) entry))))
               
               (setf (aref root-entries resize-border) nil)
               (when (zerop resize-border)
                 (setf root-entries new-root-entries
                       resize-border (fixash (length new-root-entries) +PER-ARC-BIT-LENGTH+)
                       root-bitlen  new-root-bitlen))))))))

(defun set-impl (key value hamt)
  (declare #.*interface* (hamt hamt))
  (with-slots (test hash entry-count resize-border) hamt
    (declare #.*fastest*)
    (let ((in (new-arc-stream key :hash hash)))
      (declare (dynamic-extent in))
      (find-entry-case (entry entry-place) (in hamt) 
        :on-failed (progn (incf entry-count)
                          (setf entry-place (make-key/value key value))
                          (amortized-resize hamt))
        :on-succeeded
        (if (funcall test key (k/v-key entry))
            (setf (k/v-value entry) value)
          (progn 
            (incf entry-count)
            (resolve-collision (make-key/value key value) entry
                               (arc-stream-start in) (arc-stream-rehash-count in)
                               (setf entry-place (make-amt-node))
                               hamt)
            (amortized-resize hamt)))))))
                       
(defsetf get (key hamt) (new-value)
  `(progn (set-impl ,key ,new-value ,hamt)
          ,new-value))