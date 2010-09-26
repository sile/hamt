(in-package :hamt)

(declaim (inline make-key/value))

(defstruct (key/value (:conc-name k/v-)
                      (:constructor make-key/value (key value)))
  key
  value)

(defstruct hamt
  (root-entries #() :type simple-vector)
  (root-bitlen 0    :type fixnum-length)
  (test #'equal     :type function)       
  (hash #'sxhash    :type function)
  (rehash #'rehash  :type function))

(defun make (&key (test #'equal) (hash #'sxhash) (rehash #'rehash) (root-size 32))
  (declare (function test hash rehash))
  (let ((bit-length (ceiling (log root-size 2))))
    (make-hamt :root-entries (make-array (round (expt 2 bit-length)) :initial-element nil)
               :root-bitlen bit-length
               :test test :hash hash :rehash rehash)))

(defun get (key hamt)
  (declare #.*interface* (hamt hamt))
  (let ((root-entries (hamt-root-entries hamt))
        (root-bitlen  (hamt-root-bitlen hamt))
        (hash         (hamt-hash hamt))
        (rehash       (hamt-rehash hamt)))
    (declare #.*fastest*)
    (let ((in (new-arc-stream key :hash hash :rehash rehash)))
      (declare (dynamic-extent in))
      (loop WITH node = nil
            FOR arc   = (read-n-arc in root-bitlen) THEN (read-arc in)
            FOR entry = (aref root-entries arc) THEN (get-entry node arc)
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
  (with-slots (test hash rehash) hamt
    (with-fields (key value) kv1
      (with-fields (key value) kv2
        (let ((in1 (new-arc-stream kv1.key :hash hash :rehash rehash 
                                           :start arc-start 
                                           :rehash-count rehash-count))
              (in2 (new-arc-stream kv2.key :hash hash :rehash rehash
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


(defun set-impl (key value hamt)
  (declare #.*interface* (hamt hamt))
  (with-slots (root-entries root-bitlen test hash rehash) hamt
    (declare #.*fastest*)
    (let ((in (new-arc-stream key :hash hash :rehash rehash)))
      (declare (dynamic-extent in))
      (loop WITH node = nil
            FOR arc   = (read-n-arc in root-bitlen) THEN (read-arc in)
            FOR entry = (aref root-entries arc) THEN (get-entry node arc)
        DO
        (typecase entry
          (null      (return (if node
                                 (set-entry node arc (make-key/value key value))
                               (setf (aref root-entries arc) (make-key/value key value)))))
          (amt-node  (setf node entry))
          (key/value 
           (return (if (funcall test key (k/v-key entry))
                       (setf (k/v-value entry) value)
                     (resolve-collision (make-key/value key value) entry
                                        (arc-stream-start in) (arc-stream-rehash-count in)
                                        (if node 
                                            (set-entry node arc (make-amt-node))
                                          (setf (aref root-entries arc) (make-amt-node)))
                                        hamt)))))))))

(defsetf get (key hamt) (new-value)
  `(progn (set-impl ,key ,new-value ,hamt)  
          ,new-value))