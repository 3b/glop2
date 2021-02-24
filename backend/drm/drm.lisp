;;; GLOP implementation
(in-package glop2/backend-drm)
;; see:
;; https://github.com/malcolmstill/cepl.drm-gbm/blob/master/cepl.drm-gbm.lisp
;; https://github.com/malcolmstill/ulubis-drm-gbm/blob/master/ulubis-drm-gbm.lisp
;; https://github.com/ds-hwang/gbm_es2_demo
;; https://github.com/eyelash/tutorials/blob/master/drm-gbm.c
;; https://github.com/dvdhrm/docs/tree/master/drm-howto

;; where we look for DRI devices
(defparameter *card-path* #P"/dev/dri/card*")

(defun default-card ()
  (first (directory *card-path*)))

(defclass dbus-bus ()
  ((connection :initarg :connection :reader connection)
   (bus :initarg :bus :reader bus)
   (login1 :initarg :login1 :Reader login1)
   (session :initform nil :accessor session)))

(defun open-bus (event-base)
  (format t "open bus~%")
  (let* ((server-addresses (dbus:system-server-addresses))
         (connection (dbus:open-connection event-base server-addresses
                                           :if-failed :error))
         (authenticated (dbus:authenticate
                         (dbus:supported-authentication-mechanisms connection)
                         connection))
         (bus (make-instance 'dbus:bus :name (dbus:hello connection)
                                       :connection connection)))
    (declare (ignorable authenticated))
    (print
     (make-instance 'dbus-bus
                    :connection connection
                    :bus bus
                    :login1 (dbus:make-object-from-introspection
                             (dbus:bus-connection bus)
                             "/org/freedesktop/login1"
                             "org.freedesktop.login1")))))

(defun set-session (bus session)
  (format t "add session ~s~%" session)
  (setf (session bus)
        (print
         (dbus:make-object-from-introspection
          (dbus:bus-connection (bus bus)) session
          "org.freedesktop.login1"))))

(defun application-bus ()
  (bus (glop2::window-backend-connection glop2::*application*)))

(defvar *bus*)

(defun s-call (interface method &rest args)
  (format t "  call ~s ~s: ~s~%" interface method args)  (finish-output)
  (format t "  session = ~s~%" (session *bus*))
  (let ((r (multiple-value-list (apply #'dbus:object-invoke
                                       (session *bus*)
                                       interface method args))))
    (format t "  -> ~s~%" r)
    (values-list r)))

(defun l1-call (interface method &rest args)
  (format t "  call ~s ~s: ~s~%" interface method args)  (finish-output)
  (format t "  l1 = ~s~%" (login1 *bus*))
  (let ((r (multiple-value-list (apply #'dbus:object-invoke
                                       (login1 *bus*)
                                       interface method args))))
    (format t "  -> ~s~%" r)
    (values-list r)))

(defun close-bus (bus)
  (dbus:close-connection (connection bus)))

;; (these are global so callbacks can see them more easily)
;;; mapping of int -> window, for linking windows to drm events
(defvar *id->window* (make-hash-table))
;;; next available window ID
(defvar *next-win-id* 0)

(defclass drm-backend-connection (glop2-backend:backend-connection)
  ;; device FD
  ((device :initarg :device :reader device)
   ;; name of device used to create FD ("/dev/dri/card0" etc)
   (device-name :initarg :device-name :reader device-name)
   (session :initarg :session :reader session)
   ;; iolib event-base for tracking various FDs
   (event-base :initarg :event-base :reader event-base)
   ;; dbus connection used for session management etc
   (bus :initarg :bus :reader bus)
   ;; drm event handling callbacks
   (drm-event-context :initform (make-drm-event-context) :reader drm-event-context)
   ;; libinput stuff
   (input :initform nil :reader input)
   ;; event watcher thread, since libinput wants us to read things quickly
   (event-thread :initform (start-event-thread) :reader event-thread)
   ;; for now need to buffer events to match existing code
   (events :initform nil :accessor events)
   ;; assuming just 1 window for now, so we can build events
   (win :initform nil :accessor win)))
#++(dbus:with-open-bus (*bus* (dbus:system-server-addresses)))

(defclass platform-window-drm (glop2::platform-window
                               glop2::swap-interval-mixin)
  ((pixel-format :accessor pixel-format)
   (display-config :initarg :display-config :reader display-config)
   #++(id :accessor id)
   ;; libgbm stuff (possibly belongs in platform-drm-window?
   (gbm-device :initarg :gbm-device :reader gbm-device)
   (display :initarg :display :reader display)
   (context :initarg :context :reader context)
   (gbm-surface :initarg :gbm-surface :reader gbm-surface)
   (egl-surface :initarg :egl-surface :reader egl-surface)
   ;; swap buffers stuff
   (bo :initform nil :accessor bo)
   (fb :initform 0 :accessor fb)
   ;; small integer used to map page flip events back to windows
   ;; waiting on flip
   (id :initarg :id :reader id)
   ;; flag indicating we scheduled a page flip, so shouldn't try again
   ;; until it is done
   (flipping :initform nil :accessor flipping)
   ;; buffers/pointers for "framebuffer" api
   (fb-vector :reader fb-vector)))

(defmethod window-backend-connection ((w platform-window-drm))
  (glop2::window-backend-connection (glop2::application w)))

(defmethod (setf glop2:x) (new (o platform-window-drm))
  (error "can't set drm window pos yet"))
(defmethod (setf glop2:y) (new (o platform-window-drm))
  (error "can't set drm window pos yet"))
(defmethod (setf glop2:width) (new (o platform-window-drm))
  (error "can't set drm window size yet"))
(defmethod (setf glop2:height) (new (o platform-window-drm))
  (error "can't set drm window size yet"))
#++
(defclass drm-cursor (glop2-backend:platform-cursor)
  ;; does drm have cursors?
  ())

(defclass drm-monitor (glop2:monitor)
  ((plist :reader plist :initarg :plist)))

(defmethod initialize-instance :after ((o drm-monitor) &key plist)
  (setf (slot-value o 'glop2:x) (getf plist :X))
  (setf (slot-value o 'glop2:y) (getf plist :y))
  (setf (slot-value o 'glop2:width) (getf plist :width))
  (setf (slot-value o 'glop2:height) (getf plist :height))
  (setf (slot-value o 'glop2:width-mm) (getf plist :vertical-size))
  (setf (slot-value o 'glop2:height-mm) (getf plist :horizontal-size)))


(defun set-io-handlers (connection)
  (declare (ignorable connection))
  #++
  (iolib:set-io-handler (event-base connection) (device connection)
   :read (lambda (fd event error)
           (declare (ignorable fd event))
           (when error
             (error "Connection I/O error: ~S." error))
           (drm:handle-event fd (drm-event-context connection)))))


(defvar *blanks* 0)
#++
(defun %vblank-handler (win seq time)
  (declare (ignorable win seq time))
  (incf *blanks*)
  ;; todo: call vblank handler on user window if requested?
  )

(defun %page-flip-handler (win seq time)
  (declare (ignorable win seq time))
  ;(format t ".") (finish-output)
  (setf (flipping win) nil)
  (let ((etd (event-thread (window-backend-connection win))))
    (bt:signal-semaphore (flip-semaphore etd))
    #++(bt:condition-notify (flip-cond etd))))

(defvar *%blanks* 0)
(cffi:defcallback %%vblank-handler :void ((fd :int)
                                          (seq :unsigned-int)
                                          (sec :unsigned-int)
                                          (usec :unsigned-int)
                                          (user-data (:pointer :void)))
  (declare (ignorable fd seq sec usec user-data))
  ;; todo: locking/ thread-safe hash tables? (not really expecting
  ;; multiple windows though, and even less so adding/removing them
  ;; while running)
  (incf *%blanks*)
  #++
  (let ((win (gethash (cffi:pointer-address user-data) *id->window*)))
    (when win
      (%vblank-handler win seq (+ sec (/ usec 1000000d0))))))

(defvar *flips* 0)

(cffi:defcallback %%page-flip-handler :void ((fd :int)
                                             (seq :unsigned-int)
                                             (sec :unsigned-int)
                                             (usec :unsigned-int)
                                             (user-data (:pointer :void)))
    (declare (ignorable fd seq sec usec user-data))
  ;; todo: locking/ thread-safe hash tables? (not really expecting
  ;; multiple windows though, and even less so adding/removing them
  ;; while running)
  (incf *flips*)
  (let ((win (gethash (cffi:pointer-address user-data) *id->window*)))
    (when win
      (%page-flip-handler win seq (+ sec (/ usec 1000000d0))))))

(defun make-drm-event-context ()
  (let ((p (cffi:foreign-alloc '(:struct drm:event-context))))
    (cffi:with-foreign-slots ((drm:version
                               drm:vblank-handler
                               drm:page-flip-handler)
                              p (:struct drm:event-context))
      (setf drm:version 2)
      (setf drm:vblank-handler (cffi:callback %%vblank-handler))
      (setf drm:page-flip-handler (cffi:callback %%page-flip-handler)))
    p))


(defmethod glop2::make-backend-connection ((backend
                                            (eql 'platform-window-drm)))
  #++
  (let ((eb (make-instance 'iolib:event-base))
        (card (default-card))
        (session (default-session)))
    (take-control)
    (multiple-value-bind (fd paused) (take-device session)
      (when paused
        (format t "Activate session...~%")
        (l1-call "org.freedesktop.login1.Session" "Activate"))
      (make-instance 'drm-backend-connection
                     :bus (open-bus eb :session session)
                     :event-base eb
                     :device-name card
                     :device fd
                     :session session
;;; todo: list 'connectors' to get connected monitors
                     ;; :monitors ?
                     ;; :primary-monitor ?

                     ;; :desktop ?

                     )))
  (let* ((eb (make-instance 'iolib:event-base)))
    (make-instance 'drm-backend-connection
                   :bus (open-bus eb)
                   :event-base eb
                   :device-name (print (default-card))
;;; todo: list 'connectors' to get connected monitors
                   ;; :monitors ?
                   ;; :primary-monitor ?
                   ;; :desktop ?
                   )))

(defvar *cursor-remap*
  '((:default :arrow)
    (:size-n :size-ns)
    (:size-e :size-ew)
    (:size-s :size-ns)
    (:size-w :size-ew)
    (:size-ne :size-nesw)
    (:size-nw :size-nwse)
    (:size-se :size-nwse)
    (:size-sw :size-nesw)
    (:i-beam-vertical :i-beam)
    (:cell :cross)
    (:grab :arrow)    ;; open hand
    (:move :size-all) ;; closed hand
    (:move-horizontal :size-ew)
    (:move-vertical :size-ns)))

(defun foo ()
  (let ((o (glop2::window-backend-connection glop2::*application*)))
    (when (input o)
      (destroy-input (input o)))
    (setf (slot-value o 'input) (init-input-libinput o))))

(defmethod initialize-instance :after ((o drm-backend-connection) &key session)
  (format t "init backend~%")
  (let* ((bus (print (bus o)))
         (session (or session (default-session :bus bus))))
    (format t "session = ~s~%" session)
    (set-session bus session)
    (setf (slot-value o 'session) session)
    (format t "take control~%")
    (take-control :bus bus)
    (format t "take device~%")
    (multiple-value-bind (fd paused) (take-device (device-name o) :bus bus)
      (format t "got fd ~s, paused=~s~%" fd paused)
      (when paused
        (format t "Activate session...~%")
        (let ((*bus* bus))
          (s-call "org.freedesktop.login1.Session" "Activate")))
      (setf (slot-value o 'device) fd)))
  (setf (slot-value o 'input) (init-input-libinput o))
  #++(set-io-handlers o)
  ;; add FDs to epoll
  (add-fd-to-event-loop (event-thread o)
                        (device o)
                        :tag :drm
                        :thunk (lambda ()
                                 (drm:handle-event (device o)
                                                   (drm-event-context o))
                                 ;; all processing is handled by thunk
                                 t))
  (let ((input (input o)))
   (add-fd-to-event-loop (event-thread o)
                         (libinput-fd input)
                         :tag :input
                         :thunk (lambda ()
                                  (libinput:dispatch (context input))
                                  ;; need to send message to main thread
                                  nil)))

  

  
  (format t "load cursors~%") (finish-output)
  #++
  (loop for (gcn wcn) in '((:arrow :idc-arrow)
                           (:wait :idc-wait)
                           (:wait-arrow :idc-app-starting)
                           (:cross :idc-cross)
                           (:hand :idc-hand)
                           (:size-all :idc-size-all)
                           (:size-ns :idc-size-ns)
                           (:size-ew :idc-size-we)
                           (:size-nesw :idc-size-nesw)
                           (:size-nwse :idc-size-nwse)
                           (:i-beam :idc-i-beam)
                           (:no :idc-no)
                           (:up-arrow :idc-up-arrow)
                           (:help :idc-help))

        for c = (load-cursor (cffi:null-pointer) wcn)
        when (cffi:null-pointer-p c)
          do (warn "couldn't load cursor ~s (~s)?" gcn wcn)
        else
          do (setf (gethash gcn (glop2:cursors o)) c))
  (loop for (from to) in *cursor-remap*
        do (setf (gethash from (glop2:cursors o))
                 (gethash to (glop2:cursors o))))
  #++ (setf (gethash :default (glop2:cursors o))
            (gethash :arrow (glop2:cursors o))))

(defmethod glop2::destroy-backend-connection ((backend drm-backend-connection))
  #++(let ((cursors (alexandria:hash-table-values (glop2:cursors backend))))

       (clrhash (glop2:cursors backend))
    (format t "delete cursors~%")
    (map nil 'destroy-cursor cursors))
  #++(format t "unregister class ~s~%" (class-name backend))
  #++(print (unregister-class (class-name backend) (module-handle backend)))


  ;; shut down event loop
  (shutdown-event-thread (shiftf (slot-value backend 'event-thread) nil))
  ;; clean up libinput stuff (needs dbus to release devices),
  ;; including releasing input FDs
  (destroy-input (shiftf (slot-value backend 'input) nil))
  ;; release drm FD
  (release-device (shiftf (slot-value backend 'device) nil))
  ;; release session
  (release-control)
  ;; shut down dbus connection
  (close-bus (bus backend))
  ;; clean up the iolib multiplexer
  (close (event-base backend))
  ;; free drm event callback struct
  (when (drm-event-context backend)
    (cffi:foreign-free (shiftf (slot-value backend 'drm-event-context) nil))))


(defvar *w* nil)
(defun %poll-events (bc &key blocking)
  (setf *w* bc)
  ;; check for events
  (if (events bc)
      (pop (events bc))
      (progn
        (setf
         (events bc)
         (loop
           append (multiple-value-bind (ev received)
                      (chanl:recv (messages (event-thread bc))
                                  :blockp blocking)
                    (when (not received)
                      (loop-finish))
                    (cond
                      ((atom ev)
                       (case ev
                         (:drm
                          )
                         (:input
                          (dispatch-input-events bc))))
                      (t (format t "got unexpected state event state ~s?~%"
                                 ev))))))
        (pop (events bc)))))

(defmethod glop2:%next-event ((bc drm-backend-connection) &key blocking)
  (declare (ignorable blocking))
  (%poll-events bc)
  ;; todo: send events to user
  )





(glop2::add-window-system-backend 'platform-window-drm :gl :gles :null)
(glop2::add-window-system-default-backend 'platform-window-drm :gl :gles :null)

(defun %set-cursor (w)
  (declare (ignorable w))
  #++
  (set-cursor
   (or (gethash (glop2::%cursor w)
                (glop2-backend::cursors (glop2::application w)))
       (gethash :arrow
                (glop2-backend::cursors (glop2::application w))))))
(defmethod (setf glop2:cursor) :after (cursor-name
                                       (window platform-window-drm)
                                       &key pointer)
  (declare (ignore pointer))
  (%set-cursor window))


#++
(defmethod list-video-modes ()
  (list-video-modes))

#++
(defmethod set-video-mode ((mode video-mode))
  (set-video-mode mode))

#++
(defmethod current-video-mode ()
  (current-video-mode))
(defmethod window-backend-connection ((w platform-window-drm))
  (glop2::window-backend-connection (glop2::application w)))


(defmethod window-created-hook (win))

(defun crtc-id (crtc)
  (cffi:foreign-slot-value crtc '(:struct drm:mode-crtc) 'drm:crtc-id))

(defun swap-buffers (win)
  (let ((win win)
        (drm (window-backend-connection win)))
    ;; wait for any previous flip to finish
    (progn ; when (flipping win)
      ;(format t "w") (finish-output)
      (let ((etd (event-thread drm)))
        (with-simple-restart (continue "stop waiting for page flip")
          (unless
              (bt:wait-on-semaphore (flip-semaphore etd) :timeout 0.1)
            (format t "t"); (force-output)
            (return-from swap-buffers nil))
          #++
          (bt:with-lock-held ((flip-lock etd))
            (loop for w = (bt:condition-wait (flip-cond etd) (flip-lock etd)
                                             :timeout 1000)
                  while (and w (flipping win))))))
      #++(loop repeat 1000 ;; failsafe
               do (%poll-events drm)
               do (sleep 0.001)
               while (flipping win)))
    (egl:swap-buffers (display win) (egl-surface win))
    (when (flipping win)
      (format t "x"); (force-output)
      ;(sleep 0.1)
      ;(setf (flipping win) nil)
      ;;(warn "failed to wait for page flip?")

      #++(return-from swap-buffers nil))

    (let* ((surf (gbm-surface win))
           (fd (device drm))
           (bo (gbm:surface-lock-front-buffer surf))
           (handle (gbm:bo-get-handle bo))
           (stride (gbm:bo-get-stride bo))
           ;;(connector-id (drm:connector-id (display-config win)))
           #++(mode-info (drm:mode-info (display-config win)))
           (crtc (drm:crtc (display-config win)))
           (crtc-id (crtc-id crtc)))
      (cffi:with-foreign-objects ((fb :uint32))
        (drm:mode-add-framebuffer fd (glop2:width win) (glop2:height win)
                                  24 32 stride handle fb)
        (drm:mode-page-flip fd crtc-id (cffi:mem-aref fb :uint32)
                            (cffi:foreign-bitfield-value 'page-flip-flags :event)
                            (cffi:make-pointer (id win)))
        (setf (flipping win) t)
        (when (bo win)
          (drm:mode-remove-framebuffer fd (fb win))
          (gbm:surface-release-buffer surf (bo win)))
        (setf (bo win) bo)
        (setf (fb win) (cffi:mem-ref fb :uint32))))))


(cffi:defbitfield (page-flip-flags :uint32)
  (:event 1)
  (:async 2))

(cffi:defcenum (drm-caps :uint64)
  (:dumb-buffer #x1)
  (:vblank-high-crtc #x2)
  (:dumb-preferred-depth #x3)
  (:dumb-prefer-shadow #x4)
  (:prime #x5)
  (:timestamp-monotonic #x6)
  (:async-page-flip #x7)
  (:cursor-width #x8)
  (:cursor-height #x9)
  (:addfb2-modifiers #x10)
  (:page-flip-target #x11))

(cffi:defcfun ("drmGetCap" %drm-get-cap) :int
  (fd :int)
  (cap drm-caps)
  (value (:pointer :uint64)))

(defun drm-get-cap (fd cap)
  (cffi:with-foreign-objects ((v :uint64))
    (%drm-get-cap fd cap v)
    (cffi:mem-ref v :uint64)))

(defun fionread (fd)
  (cffi:with-foreign-objects ((v :int))
    (setf (cffi:mem-ref v :int) -1)
    (let ((r (nix:ioctl fd nix:fionread v)))
      (list r (cffi:mem-ref v :int)))))

(defun fread (fd n)
  (cffi:with-foreign-object (b :uint8 n)
    (nix:read fd b n)
    (let ((v (make-array n :element-type '(unsigned-byte 8))))
      (loop for i below n
            do (setf (aref v i)
                     (cffi:mem-aref b :uint8 i)))
      v)))

(defun egl-get-error ()
  (let ((e (egl:get-error)))
    (or (cffi:foreign-enum-keyword 'egl::eglenum e :errorp nil)
        e)))

(defun check-error ()
  (let ((a (egl-get-error)))
    (unless (Eql a :success)
      (break "error ~s?" a))))

(defmethod initialize-instance :after ((o platform-window-drm) &key)
  (setf (slot-value o 'id) (incf *next-win-id*))
  (setf (gethash (id o) *id->window*) o))


(defmethod glop2:open-window ((win platform-window-drm)
                              title width height
                              &key (x 0) (y 0)
                                parent background)
  (declare (ignore x y width height parent background))
  ;; todo: figure out what to do with width/height?
  ;; ignore? find closest mode? use part of screen?

  (let ((drm (window-backend-connection win)))
    (setf (win drm) (glop2:window win)))

  (let ((fd (device (window-backend-connection win))))
    (setf (slot-value win 'display-config) (drm:find-display-configuration fd))
    (let* ((mode-info (drm:mode-info (display-config win))))
      (setf (slot-value win 'gbm-device) (gbm:create-device fd))
      (setf (slot-value win 'display)
            (egl::get-platform-display-ext :platform-gbm-khr
                                           (gbm-device win)
                                           (cffi:null-pointer)))
      (format t "egl init ~s~%" (multiple-value-list
                                 (egl:initialize (display win))))
      (egl:bind-api :opengl-es-api)
      (format t "egl api ~s~%" (egl:query-api))
      (loop for i in '(:client-apis :vendor :version :extensions)
            do (format t "~s: ~s~%"
                       i (egl:query-string (display win) i)))
      (let* ((config (car
                      (print
                       (egl::choose-config* (display win)
                                            :buffer-size 32
                                            :depth-size :dont-care
                                            :stencil-size :dont-care
                                            :surface-type :window-bit
                                            :renderable-type :opengl-es2-bit
                                            :none)))))
        (print (egl:get-config-attribs (display win) config))
        (setf (slot-value win 'context)
              (egl:create-context (display win)
                                  config
                                  (cffi:null-pointer)
                                  :context-major-version 2
                                  :none))
        (setf (slot-value win 'gbm-surface)
              (print (gbm:surface-create (gbm-device win)
                                         (drm-mode-width mode-info)
                                         (drm-mode-height mode-info)
                                         875713112 ;; xrgb8888
                                         5 ;; scanout | rendering
                                         )))
        (setf (slot-value win 'egl-surface)
              (egl:create-window-surface
               (display win) config (gbm-surface win)
               :none))
        (setf (slot-value win 'width) (drm-mode-width mode-info))
        (setf (slot-value win 'height) (drm-mode-height mode-info))
        (let ((a (egl:make-current (display win) (egl-surface win)
                                   (egl-surface win) (context win))))
          (when (zerop a)
            (break "make-current failed ~s?" (egl:get-error))))
        (loop
          for i in '(:width :height
                     :horizontal-resolution :vertical-resolution
                     :render-buffer :swap-behavior)
          do (format t "  ~s = ~s~%" i
                     (egl:query-surface (display win) (egl-surface win) i))))))
  (window-created-hook win)
  win)

(defmethod glop2:close-window ((win platform-window-drm))
  (format t "destroy-window ~s~%" win)
  ;; remove from event callback map
  (remhash (id win) *id->window*)
  ;; restore crtc
  (let* ((crtc (drm:crtc (display-config win)))
         (drm (window-backend-connection win))
         (device (device drm)))
    (when crtc
      (cffi:with-foreign-object (connector-id :uint32)
        (setf (cffi:mem-ref connector-id :uint32)
              (drm::connector-id (display-config win)))
        (cffi:with-foreign-slots ((drm:crtc-id drm:buffer-id drm:x drm:y)
                                  crtc
                                  (:struct drm:mode-crtc))
          (drm:mode-set-crtc device
                             drm:crtc-id drm:buffer-id drm:x drm:y
                             connector-id 1
                             (cffi:foreign-slot-pointer
                              crtc '(:struct drm:mode-crtc) 'drm:mode))
          (drm:mode-free-crtc (shiftf (drm:crtc (display-config win)) nil)))))
    ;; clean up old FB/BO
    (when (bo win)
      (drm:mode-remove-framebuffer device  (shiftf (fb win) nil))
      (gbm:surface-release-buffer (gbm-surface win) (shiftf (bo win) nil)))
    ;; destroy surfaces
    (egl:destroy-surface (display win)
                         (shiftf (slot-value win 'egl-surface) nil))
    (gbm:surface-destroy (shiftf (slot-value win 'gbm-surface) nil))
    ;; destroy context
    (egl:destroy-context (display win) (shiftf (slot-value win 'context) nil))
    ;; close display
    (egl:terminate (shiftf (slot-value win 'display) nil))
    ;; close gbm device
    (gbm:device-destroy (shiftf (slot-value win 'gbm-device) nil))))

#++
(defmethod set-fullscreen ((win platform-window-drm) &optional (state (not (window-fullscreen win))))
  ;; always fullscreen in drm?
  )


(defmethod glop2:set-geometry ((win platform-window-drm) x y width height)
  (error "can't modify geometry of drm windows yet"))

(defmethod glop2:show-window ((win platform-window-drm))

  )

(defmethod glop2:hide-window ((win platform-window-drm))

  )

(defmethod glop2:set-window-title ((win platform-window-drm) title)
  (setf (slot-value win 'title) title))

(defmethod glop2:swap-buffers ((win platform-window-drm))
  (swap-buffers win))

(defmethod glop2:show-cursor ((win platform-window-drm) &key pointer)
  (declare (ignore pointer))
  #++(show-cursor 1))

(defmethod glop2:hide-cursor ((win platform-window-drm) &key pointer)
  (declare (ignore pointer))
  #++(show-cursor 0))

#++
(defmethod glop2-backend:set-cursor ((win platform-window-drm)
                                     (cursor drm-cursor))
  (set-cursor cursor (hcursor cursor)))

(defmethod glop2:get-cursor-position-using-backend ((application drm-backend-connection) &key pointer)
  (declare (ignore pointer))
  #++
  (get-physical-cursor-pos))

(defmethod glop2:get-cursor-position ((win platform-window-drm) &key pointer)
  (declare (ignore pointer))
  (glop2:get-cursor-position-using-backend win))

(defmethod glop2:set-cursor-position-using-backend ((application drm-backend-connection) x y &key pointer)
  (declare (ignore pointer))
  #++(set-physical-cursor-pos x y))

(defmethod glop2:get-cursor-position ((win platform-window-drm) &key pointer)
  (declare (ignore pointer))
  (glop2:get-cursor-position-using-backend win))

#++
(defmethod glop2:%next-event ((win platform-window-drm) &key blocking)
  (let ((evt (next-event win (id win) blocking)))
    (setf %event% nil)
    evt))


(defmethod %swap-interval ((win platform-window-drm) interval)
  (unless (glop2:swap-interval-tear win)
    (setf interval (abs interval)))
  #++
  (if (and (cffi:pointerp (glop2:swap-interval-function win))
           (not (cffi:null-pointer-p (swap-interval-function win))))
      (cffi:foreign-funcall-pointer (swap-interval-function win) () :int
                                    interval :int)))

#++
(defmethod %init-swap-interval ((win platform-window-drm))
  ;; assumes we have a valid GL context...
  (let* ((dwm (%init-dwm win))
         (ext (get-extensions))
         (wesc (position "WGL_EXT_swap_control" ext :test 'string-equal))
         (wesct (position "WGL_EXT_swap_control_tear" ext :test 'string-equal)))
    (if wesc
        (setf (swap-interval-function win)
              (glop:gl-get-proc-address "wglSwapIntervalEXT"))
        (setf (swap-interval-function win)
              :unsupported))
    (setf (swap-interval-tear win) (not (not wesct))) ;; convert pos to boolean
    (if dwm
        ;; disable swap-interval if we are using dwm
        (%swap-interval win 0)
        (%swap-interval win 1))
    ;; set that we want vsync by default
    (setf (swap-interval win) 1)))

(defmethod (setf swap-interval) (interval (win platform-window-drm))
  (setf (slot-value win 'swap-interval) interval)
  (%swap-interval win interval))


(defmethod glop2:raise-window ((w platform-window-drm))
  #++
  (glop2/ffi-drm:bring-window-to-top w))

(defmethod glop2:lower-window ((w platform-window-drm))
  #++
  (glop2/ffi-drm:set-window-pos w glop2/ffi-drm:+hwnd-bottom+
                                  0 0 0 0
                                  '(:swp-no-move :swp-no-size)))


(defclass platform-pixmap-drm (dc-mixin)
  ((bmp :reader bmp :initarg :bmp)
   (width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (old :reader old :initarg :old)))

(defmethod glop2::create-pixmap ((bc drm-backend-connection) w h)
  #++
  (let* ((dc (glop2/ffi-drm:get-dc (cffi:null-pointer)))
         (cdc (create-compatible-dc dc))
         (bmp (glop2/ffi-drm-gdi32:create-compatible-bitmap dc w h))
         (old (select-object cdc bmp)))
    (unless (cffi:null-pointer-p dc)
      (unwind-protect
           (make-instance
            'platform-pixmap-drm
            :bmp bmp
            :width w
            :height h
            :dc cdc
            :old old)
        (glop2/ffi-drm:release-dc (cffi:null-pointer) dc)))))

(defmethod glop2:create-pixmap ((pw platform-window-drm) w h)
  #++
  (let* ((dc (dc pw))
         (cdc (create-compatible-dc dc))
         (bmp (glop2/ffi-drm-gdi32:create-compatible-bitmap dc w h))
         (old (select-object cdc bmp)))
    (unless (cffi:null-pointer-p dc)
      (make-instance
       'platform-pixmap-drm
       :bmp bmp
       :width w
       :height h
       :dc cdc
       :old old))))

(defmethod glop2:free-pixmap (wbc (pm platform-pixmap-drm))
  #++
  (when (old pm)
    (select-object (dc pm) (shiftf (slot-value pm 'old) nil)))
  #++
  (when (bmp pm)
    (delete-object (shiftf (slot-value pm 'bmp) nil)))
  #++
  (when (dc pm)
    (delete-object (shiftf (slot-value pm 'dc) nil))))

#++
(defmethod glop2:free-pixmap ((bc drm-backend-connection) pixmap)
  (glop2/ffi-drm-gdi32:delete-object pixmap))

(defmethod glop2:flush ((bc drm-backend-connection))

  )

(defmethod glop2:get-cursor-pos ((bc drm-backend-connection))

  )

(defmethod glop2::%blit-pointer ((win platform-window-drm)
                                 pointer x y w h)
)

(defmethod glop2::blit ((from dc-mixin) fx fy w h
                        (to dc-mixin) tx ty)
  (format *debug-io* "blit ~sx~s from ~s,~s to ~s,~s~%" w h fx fy tx ty))




