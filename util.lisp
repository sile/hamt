(in-package :hamt)

(defun symb (&rest args)
  (intern
   (with-output-to-string (out)
     (dolist (a args) (princ a out)))))

(defmacro with-fields ((&rest fields) instance &body body)
  `(with-slots ,(mapcar (lambda (f) `(,(symb instance"."f) ,f)) fields)
               ,instance
     ,@body))

(defun hash (key rehash-count)
  (declare #.*fastest* (positive-fixnum rehash-count))
  (if (zerop rehash-count)
      (sxhash key)
    (sxhash (format nil "~D~A" rehash-count key))))
