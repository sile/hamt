(in-package :hamt)

(declaim (inline alloc-entries free-entries))

(defstruct (entries-pool (:constructor make-entries-pool))
  (pool (make-array +BITMAP-SIZE+ :initial-element nil) :type simple-vector)
  (allocate-count 0                                     :type positive-fixnum))

;; ATTENTION!
;; This special variable is source of thread unsafety.
;; In multi-threaded programming, per thread must bind their original entries-pool instance to below variable.
(defvar *entries-pool* (make-entries-pool))

(defun alloc-entries (size &aux (entries-pool *entries-pool*))
  (declare #.*fastest*
           (entries-pool entries-pool)
           (bitmap-length size))
  (with-slots (allocate-count pool) entries-pool
    (when (= allocate-count #x10000)
      (fill pool nil)
      (setf allocate-count 0))
    (incf allocate-count)
    
    (if #1=(aref pool (1- size))
          (pop #1#)
      (make-array size))))

(defun free-entries (entries &aux (len (length entries)) (entries-pool *entries-pool*))
  (declare #.*fastest*
           (simple-vector entries)
           (entries-pool entries-pool))
  (when (plusp len)
    (push entries (aref (entries-pool-pool entries-pool) (1- len)))))