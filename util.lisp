(in-package :hamt)

(declaim (inline fixash))

(defun fixash (integer count)
  (declare (positive-fixnum integer)
           (fixnum-length count))
  (the positive-fixnum (ash integer count)))

(defun symb (&rest args)
  (intern
   (with-output-to-string (out)
     (dolist (a args) (princ a out)))))

(defmacro with-fields ((&rest fields) instance &body body)
  `(with-slots ,(mapcar (lambda (f) `(,(symb instance"."f) ,f)) fields)
               ,instance
     ,@body))

(defmacro post-incf (n &optional (delta 1))
  `(prog1 ,n (incf ,n ,delta)))

(defun hash (key rehash-count)
  "Default hash function for arc-stream"
  (declare #.*fastest* (positive-fixnum rehash-count)
           #.*muffle-warning*)
  (if (zerop rehash-count)
      (sxhash key)
    (sxhash (format nil "~D~A" rehash-count key))))