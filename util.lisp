(in-package :hamt)

(defun symb (&rest args)
  (intern
   (with-output-to-string (out)
     (dolist (a args) (princ a out)))))

(defmacro with-fields ((&rest fields) instance &body body)
  `(with-slots ,(mapcar (lambda (f) `(,(symb instance"."f) ,f)) fields)
               ,instance
     ,@body))
