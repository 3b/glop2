(in-package #:glop2)
#++(require 'glop2)

;;; per-os packages define subclasses of this, and add name to
;;; *window-system-backends*, then backend is selected when window is
;;; created
(defclass platform-window ()
  ((3d-api :initform :gl :initarg :3d-api :reader 3d-api)
   (x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (width :initform 512 :initarg :width :accessor width)
   (height :initform 512 :initarg :height :accessor height)
   (title :initform "glop2" :initarg :title :accessor title)
   (fullscreen :initform nil :accessor fullscreen)
   (application :reader application)
   (previous-video-mode :accessor previous-video-mode
                        :initform nil)
   (window :initarg :window :reader window)
   (cursor :accessor %cursor :initform :default)))

(defmethod cursor ((window platform-window) &key pointer)
  (declare (ignore pointer))
  (%cursor window))

(defmethod (setf cursor) (cursor-name (window platform-window) &key pointer)
  (declare (ignore pointer))
  (let ((platform-cursor
          (or (gethash cursor-name (cursors (application window)))
              :arrow)))
    (unless platform-cursor
      (error "tried to set unknown cursor ~s?" cursor-name))
    #++(set-cursor (window window) platform-cursor))
  (setf (%cursor window) cursor-name))

;;; list of names of available windowing system backends, depends on
;;; which glop2/*.asd systems were loaded
(defvar *window-system-backends* (make-hash-table))
(defvar *window-system-default-backend* (make-hash-table))
(defun add-window-system-backend (name &rest apis)
  (loop for api in apis
        do (pushnew name (gethash api *window-system-backends*))))
(defun add-window-system-default-backend (name &rest apis)
  (loop for api in apis
        do (setf (gethash api *window-system-default-backend*) name)))

(defun default-window-system-backend-for-api (api)
  (gethash api *window-system-default-backend*))

(defvar *3d-api-context-backends* (make-hash-table))
(defvar *3d-api-context-default-backend* (make-hash-table))
(defun add-context-backend (name &rest apis)
  (loop for api in apis
        do (pushnew name (gethash api *3d-api-context-backends*))))
(defun add-context-default-backend (name &rest apis)
  (loop for api in apis
        do (setf (gethash api *3d-api-context-default-backend*) name)))

(defun default-context-backend-for-api (api)
  (gethash api *3d-api-context-default-backend*))

(defclass swap-interval-mixin ()
  ((swap-interval-function :initform :uninitialized
                           :accessor swap-interval-function)
   (swap-interval-tear :accessor swap-interval-tear)))


(defclass context-backend ()
  ())
(defmethod initialize-instance :after ((o context-backend)
                                       &key &allow-other-keys))

(defclass null-context-backend (context-backend)
  ())
(defmethod initialize-instance :after ((o null-context-backend)
                                       &key &allow-other-keys))
(defmethod open-context ((cb null-context-backend) wb attribs)
  nil)
(add-context-backend 'null-context-backend :gl :gles :null)

(defparameter *default-gl-attribs*
  '(:rgba t
    :double-buffer t
    :red-size 4
    :green-size 4
    :blue-size 4
                                        ;:alpha-size 4
    :depth-size 16))

