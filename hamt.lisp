(in-package :hamt)

(declaim (inline make-key/value))

(defstruct (key/value (:conc-name k/v-)
                      (:constructor make-key/value (key value)))
  key
  value)

(defstruct hamt
  (root (make-amt-node) :type amt-node)
  (test #'equal         :type function)       
  (hash #'sxhash        :type function))

(defun make (&key (test #'equal) (hash #'sxhash))
  (declare (function test hash))
  (make-hamt :test test :hash hash))

(defun get (key hamt)
  (declare #.*fastest*
           (hamt hamt))
  (with-slots (root test hash) hamt
    (let ((in (new-arc-stream key :hash hash)))
      (declare (dynamic-extent in))
      (loop WITH node = root
            FOR arc   = (read-arc in)
            FOR entry = (get-entry node arc)
      DO
      (typecase entry
        (null     (return (values nil nil)))
        (amt-node (setf node entry))        
        (key/value                          
         (return (if (funcall test key (k/v-key entry))  
                     (values (k/v-value entry) t)
                   (values nil nil)))))))))

(defun resolve-collision (kv1 kv2 arc-start rehash-count node arc hamt)
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
          (setf node (set-entry node arc (make-amt-node)))
          (loop FOR a1 = (read-arc in1)
                FOR a2 = (read-arc in2)
                WHILE (= a1 a2) 
            DO 
            (setf node (set-entry node a1 (make-amt-node)))
            
            FINALLY
            (set-entry node a1 kv1)
            (set-entry node a2 kv2)))))))


(defun set-impl (key value hamt)
  (declare #.*fastest*
           (hamt hamt))
  (with-slots (root test hash) hamt
    (let ((in (new-arc-stream key :hash hash)))
      (declare (dynamic-extent in))
      (loop WITH node = root
            FOR arc = (read-arc in)
            FOR entry = (get-entry node arc)
        DO
        (typecase entry
          (null      (return (set-entry node arc (make-key/value key value)))) 
          (amt-node  (setf node entry))
          (key/value 
           (return (if (funcall test key (k/v-key entry))
                       (setf (k/v-value entry) value)
                     (resolve-collision (make-key/value key value) entry
                                        (arc-stream-start in) (arc-stream-rehash-count in)
                                        node arc
                                        hamt)))))))))

(defsetf get (key hamt) (new-value)
  `(progn (set-impl ,key ,new-value ,hamt)  
          ,new-value))