(in-package :glop2-backend)

(defclass backend-connection ()
  ((cursors :initform (make-hash-table :test 'equalp) :reader cursors)
   ;; list of (subclasses of) glop2:monitor
   (monitors :reader monitors :initarg :monitors)
   (primary-monitor :Reader glop2:primary-monitor :initarg :primary-monitor)
   ;; glop2:monitor instance representing virtual desktop (or primary
   ;; monitor if only 1?)
   (desktop :reader glop2:desktop :initarg :desktop)))

(defclass platform-cursor ()
  ())

(defgeneric create-pixmap (bc w h))
