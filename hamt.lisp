(in-package :hamt)

(declaim (inline make-key/value))

(defstruct (key/value (:conc-name k/v-))
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
