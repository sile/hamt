(in-package :hamt)

;; (declaim (inline alloc-entries free-entries))
(declaim (notinline alloc-entries free-entries))

(defstruct (entries-pool (:constructor make-entries-pool))
  (pool (make-array +BITMAP-SIZE+ :initial-element nil) :type simple-vector)
  (allocate-count 0                                     :type positive-fixnum))

(defvar *entries-pool* (make-entries-pool))

(defun alloc-entries (size entries-pool)
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

(defun free-entries (entries entries-pool)
  (declare #.*fastest*
           (simple-vector entries)
           (entries-pool entries-pool))
  (when (plusp (length entries))
    (push entries (aref (entries-pool-pool entries-pool) 
                        (1- (length entries))))))
