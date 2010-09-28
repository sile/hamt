(in-package :hamt)

(declaim (inline ctpop valid-entry-p get-entry make-amt-node make-key/value))

(defun ctpop (bitmap &key (start 0) (end +BITMAP-SIZE+))
  (declare (bitmap bitmap)
           (bitmap-length start end))
  (logcount (ldb (byte (- end start) start) bitmap)))

(defstruct (key/value (:conc-name k/v-)
                      (:constructor make-key/value (key value)))
  (key   nil :type t :read-only t)
  (value nil :type t))

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
          (declare (simple-vector new-entries))
          (setf (ldb (byte 1 arc) bitmap) 1)
          (replace new-entries entries :end1 new-entry-index :end2 new-entry-index)
          (replace new-entries entries :start1 (1+ new-entry-index) :start2 new-entry-index)
          (free-entries entries)
          (setf entries new-entries)))
      (setf (aref entries new-entry-index) new-entry))))

(defun set-new-2-entries (node arc1 entry1 arc2 entry2)
  (declare #.*fastest*
           (amt-node node)
           (arc arc1 arc2))
  (with-slots (bitmap entries) node
    (setf entries (alloc-entries 2)
          (ldb (byte 1 arc1) bitmap) 1
          (ldb (byte 1 arc2) bitmap) 1)
    (if (< arc1 arc2)
        (setf (aref entries 0) entry1
              (aref entries 1) entry2)
      (setf (aref entries 1) entry1
            (aref entries 0) entry2))))

(defsetf get-entry (node arc) (new-value)
  `(set-entry ,node ,arc ,new-value))

(defmacro each-entry ((arc entry) node &body body)
  `(loop WITH ,node = ,node
         FOR ,arc FROM 0 BELOW +BITMAP-SIZE+
         FOR ,entry = (get-entry ,node ,arc)
         WHEN ,entry
     DO
     (locally
      ,@body)))