(in-package :hamt)

(declaim (inline make-arc-stream new-arc-stream read-arc read-n-arc))

(defstruct arc-stream
  (hash #'hash    :type function :read-only t)
  (key  t         :type t        :read-only t)
  (hash-code 0    :type positive-fixnum)
  (start 0        :type arc-start)
  (rehash-count 0 :type positive-fixnum))

(defun new-arc-stream (key &key hash (start 0) (rehash-count 0))
  (declare (positive-fixnum rehash-count)
           (function hash)
           #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((hash-code (funcall hash key rehash-count)))
    (make-arc-stream :hash hash
                     :key key
                     :hash-code hash-code
                     :start start
                     :rehash-count rehash-count)))

(defun read-arc (arc-stream)
  (declare (arc-stream arc-stream))
  (with-slots (hash key hash-code start rehash-count) arc-stream
    (when (>= start +FIXNUM-LENGTH+)
      (setf hash-code (funcall hash key (incf rehash-count))
            start 0))
    (prog1 (ldb (byte +PER-ARC-BIT-LENGTH+ start) hash-code)
      (incf start +PER-ARC-BIT-LENGTH+))))

(defun read-n-arc (arc-stream bit-length)
  (declare (arc-stream arc-stream)
           (bitmap-length bit-length)
           #+SBCL (sb-ext:muffle-conditions sb-ext:compiler-note))
  (with-slots (hash key hash-code start rehash-count) arc-stream
    (when (>= start +FIXNUM-LENGTH+)
      (setf hash-code (funcall hash key (incf rehash-count))
            start 0))
    (prog1 (ldb (byte bit-length start) hash-code)
      (incf start bit-length))))