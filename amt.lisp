(in-package :hamt)

(declaim (inline ctpop valid-entry-p get-entry make-amt-node))

(defun ctpop (bitmap &key (start 0) (end +BITMAP-SIZE+))
  (declare (bitmap bitmap)
           (bitmap-length start end))
  (logcount (ldb (byte (- end start) start) bitmap)))

(defparameter *empty-entries* #()) ; XXX:

(defstruct amt-node
  (bitmap  0   :type bitmap)
  (entries *empty-entries* :type simple-vector))

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

(defparameter *entry-pool* (make-array (1+ +BITMAP-SIZE+) :initial-element nil))

(defun set-entry (node arc new-entry)
  (declare #.*fastest*
           (amt-node node)
           (arc arc))
  (with-slots (bitmap entries) node

    (let ((new-entry-index (ctpop bitmap :end arc)))
      (unless (valid-entry-p node arc)
        (unless #1=(aref (the simple-vector *entry-pool*) (1+ (length entries)))
          (push (make-array (1+ (length entries))) #1#))
        (let ((new-entries (pop #1#)))
          (declare (simple-vector new-entries))
          (setf (ldb (byte 1 arc) bitmap) 1)
          (replace new-entries entries :end1 new-entry-index :end2 new-entry-index)
          (replace new-entries entries :start1 (1+ new-entry-index) :start2 new-entry-index)
          (when (plusp (length entries))
            (push entries (aref (the simple-vector *entry-pool*) (length entries))))
          (setf entries new-entries)))
      (setf (aref entries new-entry-index) new-entry))))

