(in-package :hamt)

(declaim (inline ctpop valid-entry-p get-entry make-amt-node))

(defun ctpop (bitmap &key (start 0) (end +BITMAP-SIZE+))
  (declare (bitmap bitmap)
           (bitmap-length start end))
  (logcount (ldb (byte (- end start) start) bitmap)))

(defstruct amt-node
  (bitmap  0              :type bitmap)
  (entries +EMPTY-VECTOR+ :type simple-vector))

(defun valid-entry-p (node arc)
  (declare (amt-node node)
           (arc arc))
  (ldb-test (byte 1 arc) (amt-node-bitmap node)))

(defun get-entry (node arc)
  (declare (amt-node node)
           (arc arc))
  (with-slots (bitmap entries) node
    (if (not (valid-entry-p node arc))
        nil
      (aref entries (ctpop bitmap :end arc)))))

(defun set-entry (node arc new-entry)
  (declare #.*fastest*
           (amt-node node)
           (arc arc))
  (with-slots (bitmap entries) node
    (let ((new-entry-index (ctpop bitmap :end arc)))
      (unless (valid-entry-p node arc)
        (let ((new-entries (alloc-entries (1+ (length entries)))))
          (setf (ldb (byte 1 arc) bitmap) 1)
          (replace new-entries entries :end1 new-entry-index :end2 new-entry-index)
          (replace new-entries entries :start1 (1+ new-entry-index) :start2 new-entry-index)
          (free-entries entries)
          (setf entries new-entries)))
      (setf (aref entries new-entry-index) new-entry))))