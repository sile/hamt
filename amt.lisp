(in-package :hamt)

(declaim (inline ctpop valid-entry-p))

(defun ctpop (bitmap &key (start 0) (end +BITMAP-SIZE+))
  (declare (bitmap bitmap)
           (bitmap-length start end))
  (logcount (ldb (byte (- end start) start) bitmap)))

(defstruct amt-node
  (bitmap  0   :type bitmap)
  (entries #() :type simple-vector))

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
  (declare (amt-node node)
           (arc arc))
  (with-slots (bitmap entries) node
    (let ((new-entry-index (ctpop bitmap :end arc)))
      (unless (valid-entry-p node arc)
        (setf (ldb (byte 1 arc) bitmap) 1                          
              entries (adjust-array entries (1+ (length entries))))
        (loop FOR i FROM (1- (length entries)) DOWNTO (1+ new-entry-index)
          DO
          (setf (aref entries i) (aref entries (1- i)))))
      (setf (aref entries new-entry-index) new-entry)))) 
