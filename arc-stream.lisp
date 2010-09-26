(in-package :hamt)

(declaim (inline make-arc-stream new-arc-stream read-arc read-n-arc))

(defun rehash (hash key rehash-count)
  (funcall hash (format nil "~D~A" rehash-count key)))

(defstruct arc-stream
  (hash #'sxhash   :type function :read-only t)
  (rehash #'rehash :type function :read-only t)
  (key  t          :type t        :read-only t)
  (hash-code 0     :type positive-fixnum)
  (start 0         :type arc-start)
  (rehash-count 0  :type positive-fixnum))

(defun new-arc-stream (key &key hash (rehash #'rehash) (start 0) (rehash-count 0))
  (declare (positive-fixnum rehash-count)
           (function hash rehash)
           #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((hash-code (if (zerop rehash-count)
                       (funcall hash key)
                     (funcall rehash hash key rehash-count))))
    (make-arc-stream :hash hash
                     :rehash rehash
                     :key key
                     :hash-code hash-code
                     :start start
                     :rehash-count rehash-count)))

(defun read-arc (arc-stream)
  (declare (arc-stream arc-stream))
  (with-slots (hash rehash key hash-code start rehash-count) arc-stream
    (when (>= start +FIXNUM-LENGTH+)
      (setf hash-code (funcall rehash hash key (incf rehash-count))
            start 0))
    (prog1 (ldb (byte +PER-ARC-BIT-LENGTH+ start) hash-code)
      (incf start +PER-ARC-BIT-LENGTH+))))

(defun read-n-arc (arc-stream bit-length)
  (declare (arc-stream arc-stream)
           (bitmap-length bit-length)
           #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-slots (hash rehash key hash-code start rehash-count) arc-stream
    (when (>= start +FIXNUM-LENGTH+)
      (setf hash-code (funcall rehash hash key (incf rehash-count))
            start 0))
    (prog1 (ldb (byte bit-length start) hash-code)
      (incf start bit-length))))