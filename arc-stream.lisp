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
           #.*muffle-warning*)
  (let ((hash-code (funcall hash key rehash-count)))
    (make-arc-stream :hash hash
                     :key key
                     :hash-code hash-code
                     :start start
                     :rehash-count rehash-count)))

(defun read-n-arc (arc-stream bit-length)
  (with-slots (hash key hash-code start rehash-count) arc-stream
    (when (>= start +FIXNUM-LENGTH+)
      (setf hash-code (funcall hash key (incf rehash-count))
            start 0))
    (ldb (byte #1=bit-length (post-incf start #1#)) hash-code)))

(defun read-arc (arc-stream)
  (declare (arc-stream arc-stream))
  (read-n-arc arc-stream +PER-ARC-BIT-LENGTH+))