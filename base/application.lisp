(in-package #:glop2)

;;; to hide differences in how OS dispatch events, glop2 has an
;;; "application" abstraction. events are processed at application
;;; level, and depending on settings, either dispatched to all windows
;;; in that application, or queued for them to receive individually

;;; on windows, an 'application' corresponds to the thread that
;;; created the windows. On X, it corresponds to the 'display'
;;; connection. On OSX, you can only create windows on main thread, so
;;; it is effectively just a singleton shared by all applications.


;;; for portability, applications should do all window system
;;; interaction on a single thread (and on osx insure that is the
;;; 'main' thread).  on windows or X, multiple applications should be
;;; able to run at once on separate threads. on osx, multple
;;; applications should be able to run as long as they are started
;;; from the main thread (either by interrupting the previously
;;; running application, or by arranging for it to start the others)

;;; by default, all windows in a single application will share same
;;; window system backends and rendering APIs. Might be able to change
;;; some of those per window, but not sure how much. (for example
;;; probably can use GL and vk in same application, but probably not
;;; xlib and xcb, since theyy need to share internal event handler)

(defvar *application*)

(defclass application ()
  ((3d-api :initform :gl :initarg :3d-api :reader 3d-api)
   (window-backend :initarg :window-backend :reader window-backend)
   (context-backend :initarg :context-backend :reader context-backend)
   (window-backend-connection :initarg :window-backend-connection
                              :reader window-backend-connection)
   (pushed-events :initform nil :accessor pushed-events)
   (active-windows :initform (make-hash-table) :reader active-windows)
   ;; by default, we just dispatch all events to all windows for a
   ;; given application. optionally users can request per-window
   ;; dispatch, in which case application dispatch will add events to
   ;; per-window queue for window to pull later
   #++ todo:
   (window-event-queues :initform (make-hash-table))))

;; base class of 'monitor' abstraction, should
(defclass monitor ()
  ((x :reader x :initarg :x)
   (y :reader y :initarg :y)
   (width :reader width :initarg :width)
   (height :Reader height :initarg :height)
   (width-mm :reader width-mm :initarg :width-mm)
   (height-mm :Reader height-mm :initarg :height-mm)))

(defmethod cursors ((app application))
  (cursors (window-backend-connection app)))

(defgeneric make-backend-connection (name))

(defun make-application (&key (3d-api :gl)
                           (window-backend :default)
                           (context-backend :default)
                           (connect t))
  (when (or (not window-backend)
            (eql window-backend :default))
    (setf window-backend (default-window-system-backend-for-api 3d-api)))
  (when (or (not context-backend)
            (eql context-backend :default))
    (setf context-backend (default-context-backend-for-api 3d-api)))
  (make-instance
   'application
   :3d-api 3d-api
   :window-backend  window-backend
   :context-backend context-backend
   ;; if we already have an application connected, use its connection
   ;; if it matches this on (mainly intended for OSX, but also needed
   ;; if someone starts multiple applications on 1 thread in windows,
   ;; and might as well behave the same on X)
   :window-backend-connection (if (and (boundp '*application*)
                                       (eql window-backend
                                            (window-backend *application*)))
                                  (window-backend-connection *application*)
                                  (when connect
                                    (make-backend-connection window-backend)))))


(defmacro with-application ((&key (3d-api :gl)
                               (window-backend :default)
                               (context-backend :default))
                            &body body)
  `(with-simple-restart (quit "quit")
     (let ((*application* (make-application :3d-api ,3d-api
                                            :window-backend ,window-backend
                                            :context-backend ,context-backend)))
       (unwind-protect
            (with-simple-restart (quit "quit")
              (progn,@body))
         (when (window-backend-connection *application*)
           (destroy-backend-connection (window-backend-connection *application*)))))))


;; not sure about these APIs...
(defmethod primary-monitor ((a application))
  (primary-monitor (window-backend-connection a)))

(defmethod desktop ((a application))
  (desktop (window-backend-connection a)))

(defmethod width-in-millimeters ((m monitor))
  (width-mm m))
(defmethod height-in-millimeters ((m monitor))
  (height-mm m))

(defmethod create-pixmap ((a application) w h)
  (create-pixmap (window-backend-connection a) w h))

(defmethod free-pixmap ((a application) pixmap)
  (free-pixmap (window-backend-connection a) pixmap))

(defmethod flush ((a application))
  (flush (or (context-backend a) (window-backend-connection a))))

(defmethod get-cursor-position ((a application) &key pointer)
  (get-cursor-position-using-backend
   (window-backend-connection a)
   :pointer pointer))
