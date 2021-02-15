;;; GLOP implementation
(in-package #:glop2/backend-win32)

(defclass win32-backend-connection (glop2-backend:backend-connection)
  ((module-handle :initarg :module-handle :accessor module-handle)
   (dwm-active :initform :uninitialized :reader dwm-active)
   (class-name :initarg :class-name :reader class-name)))

#++
(defclass win32-cursor (glop2-backend:platform-cursor)
  ((hcursor :initform)))

(defclass win32-monitor (glop2:monitor)
  ((plist :reader plist :initarg :plist)))

(defmethod initialize-instance :after ((o win32-monitor) &key plist)
  (setf (slot-value o 'glop2:x) (getf plist :X))
  (setf (slot-value o 'glop2:y) (getf plist :y))
  (setf (slot-value o 'glop2:width) (getf plist :width))
  (setf (slot-value o 'glop2:height) (getf plist :height))
  (setf (slot-value o 'glop2:width-mm) (getf plist :vertical-size))
  (setf (slot-value o 'glop2:height-mm) (getf plist :horizontal-size)))

(defmethod glop2::make-backend-connection ((backend
                                            (eql 'platform-window-win32)))
  (let ((class-name "GLOP2")
        (module-handle (get-module-handle (cffi:null-pointer)))
        (monitors (glop2/ffi-win32:enum-display-monitors))
        (primary)
        (desktop
          (make-instance
           'glop2:monitor
           :width (glop2/ffi-win32:get-system-metrics :cx-virtual-screen)
           :height (glop2/ffi-win32:get-system-metrics :cy-virtual-screen)
           :x (glop2/ffi-win32:get-system-metrics :x-virtual-screen)
           :y (glop2/ffi-win32:get-system-metrics :y-virtual-screen))))
    (glop2/ffi-win32:get-system-metrics :cx-size)
    (setf monitors
          (loop for p in monitors
                for m = (make-instance 'win32-monitor
                                       :plist p)
                collect m
                when (find :primary (getf p :flags))
                  do (setf primary m)))
    ;; register window clas
    (format t "register class ~s~%" class-name)
    (create-and-register-class module-handle
                               class-name
                               'window-proc)
    (make-instance 'win32-backend-connection
                   :module-handle module-handle
                   :class-name class-name
                   :desktop desktop
                   :monitors monitors
                   :primary-monitor primary)))

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
(defmethod initialize-instance :after ((o win32-backend-connection) &key)
  (format t "load cursors~%")
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

(defmethod glop2::destroy-backend-connection ((backend win32-backend-connection))
  (let ((cursors (alexandria:hash-table-values (glop2:cursors backend))))
    (clrhash (glop2:cursors backend))
    (format t "delete cursors~%")
    (map nil 'destroy-cursor cursors))
  (format t "unregister class ~s~%" (class-name backend))
  (print (unregister-class (class-name backend) (module-handle backend))))

(defmethod glop2:%next-event ((bc win32-backend-connection) &key blocking)
  (let ((evt (next-event nil (cffi:null-pointer) blocking)))
    (when evt
      (print evt))
    (setf %event% nil)
    evt))

(defclass dc-mixin ()
  ((dc :accessor dc :initarg :dc)))

(defclass platform-window-win32 (glop2::platform-window
                                 glop2::swap-interval-mixin
                                 dc-mixin)
  ((pixel-format :accessor pixel-format)
   (id :accessor id)
   #++(in-size-move :accessor win32-window-in-size-move :initform nil
                    :accessor in-size-move)
   #++(size-event :initform nil
                  :accessor win32-window-pushed-size-event)
   ;; store desired swap interval in case we are using dwm instead
   (swap-interval :accessor swap-interval)
   ;; buffers/pointers for "framebuffer" api
   (fb-vector :reader fb-vector)))

(glop2::add-window-system-backend 'platform-window-win32 :gl :gles :null)
(glop2::add-window-system-default-backend 'platform-window-win32 :gl :gles :null)

(defun %set-cursor (w)
  (set-cursor
   (or (gethash (glop2::%cursor w)
                (glop2-backend::cursors (glop2::application w)))
       (gethash :arrow
                (glop2-backend::cursors (glop2::application w))))))
(defmethod (setf glop2:cursor) :after (cursor-name
                                       (window platform-window-win32)
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
(defmethod window-backend-connection ((w platform-window-win32))
  (glop2::window-backend-connection (glop2::application w)))
(defmethod module-handle ((w platform-window-win32))
  (module-handle (glop2::window-backend-connection (glop2::application w))))
(defmethod class-name ((w platform-window-win32))
  (class-name (glop2::window-backend-connection (glop2::application w))))
(defmethod dwm-active ((w platform-window-win32))
  (dwm-active (glop2::window-backend-connection (glop2::application w))))

(defmethod window-created-hook (win))

(defun %update-geometry-from-window (win)
  ;; update geometry from client rect of actual window
  (let ((wnd (id win)))
    (multiple-value-bind (cx cy cwidth cheight)
        (get-client-rect wnd)
      (multiple-value-bind (sx sy)
          (client-to-screen wnd cx cy)
        (glop2::%update-geometry win sx sy cwidth cheight)))))

(defvar *tree* (make-hash-table))
(defmethod glop2:open-window ((win platform-window-win32)
                              title width height
                              &key (x 0) (y 0)
                                parent background)
  (let ((style (if parent
                   '(:ws-child :ws-clip-siblings :ws-clip-children)
                   '(:ws-overlapped-window :ws-clip-siblings :ws-clip-children)))
        (ex-style (if parent
                      '()
                      '(:ws-ex-app-window :ws-ex-window-edge))))
    ;; calculate window size/position so client rect is requested size/position
    (multiple-value-bind (ax ay aw ah)
        (adjust-window-rect-ex x y width height
                               :style style
                               :ex-style ex-style)
      (setf x ax y ay width aw height ah))
    ;; create the window
    (let* ((wnd (create-window-ex ex-style
                                  (class-name win)
                                  title
                                  style
                                  x y width height
                                  (if parent
                                      (id (platform-window parent))
                                      (cffi:null-pointer))
                                  (cffi:null-pointer)
                                  (module-handle win)
                                  (cffi:null-pointer))))
      (unless wnd
        (error "Can't create window (error ~S)~%"
               (get-last-error-string)))
      (setf (id win) wnd)
      (setf (gethash (cffi:pointer-address wnd) *window-id-mapping*)
            (glop2::window win))
      (setf (gethash (glop2::window win)
                     (glop2::active-windows (glop2::application win)))
            t)
      ;; get actual client rect instead of assuming it is specified size
      (%update-geometry-from-window win)))
  (format t "add ~s -> ~s~%" parent (window win))
  (push (list (window win) (x win) (y win) (width win) (height win))
        (gethash parent *tree*))

  (setf (dc win) (get-dc (id win)))
  (window-created-hook win)
  #++
  (setf (win32-window-pixel-format win) (choose-pixel-format
                                         win
                                         :rgba rgba
                                         :double-buffer double-buffer
                                         :stereo stereo
                                         :red-size red-size
                                         :green-size green-size
                                         :blue-size blue-size
                                         :alpha-size alpha-size
                                         :depth-size depth-size
                                         :accum-buffer accum-buffer
                                         :accum-red-size accum-red-size
                                         :accum-green-size accum-green-size
                                         :accum-blue-size accum-blue-size
                                         :stencil-buffer stencil-buffer
                                         :stencil-size stencil-size))

  (set-foreground-window (id win))
  (update-window (id win))
  ;; fake initial 'resize' event since we miss some size events during
  ;; window creation
  #++(setf (window-pushed-event win)
           (make-instance 'resize-event :width (width win)
                                        :height (height win)))
  (%init-dwm win)
  win)

(defmethod glop2:close-window ((win platform-window-win32))
  (release-dc (id win) (dc win))
  (format t "destroy-window ~s~%" win)
  (print (destroy-window (id win)))

  ;; fixme: count windows using class so we only unregister it when none left?
  #++(unregister-class (class-name win)
                       (module-handle win)))

#++
(unregister-class "GLOP-GL" (get-module-handle (cffi:null-pointer)))

#++
(defmethod set-fullscreen ((win platform-window-win32) &optional (state (not (window-fullscreen win))))
  (with-accessors ((id win32-window-id)
                   (fullscreen window-fullscreen))
      win
    (unless (eq state fullscreen)
      (if state
          (progn (glop-win32::%set-fullscreen id t)
                 (setf fullscreen t))
          (progn (glop-win32::%set-fullscreen id nil)
                 (setf fullscreen nil))))
    (glop-win32:update-window id)
    (show-window win)))


(defmethod glop2:set-geometry ((win platform-window-win32) x y width height)
  (setf x (or x (x win)))
  (setf y (or y (y win)))
  (setf width (or width (width win)))
  (setf height (or height (height win)))
  (set-geometry (id win) x y width height)
  (glop2::%update-geometry win x y width height))

(defvar *first* t)
(defmethod glop2:show-window ((win platform-window-win32))
  (show-window (id win) :sw-show)
  (when *first*
    (show-window (id win) :sw-hide)
    (show-window (id win) :sw-show))
  (set-focus (id win)))

(defmethod glop2:hide-window ((win platform-window-win32))
  (show-window (id win) :sw-hide))

(defmethod glop2:set-window-title ((win platform-window-win32) title)
  (setf (slot-value win 'title) title)
  (set-window-text (id win) title))

(defmethod glop2:swap-buffers ((win platform-window-win32))
  (swap-buffers (dc win))
  (when (and (dwm-active win)
             (not (zerop (swap-interval win))))
    (dwm-flush)))

(defmethod glop2:show-cursor ((win platform-window-win32) &key pointer)
  (declare (ignore pointer))
  (show-cursor 1))

(defmethod glop2:hide-cursor ((win platform-window-win32) &key pointer)
  (declare (ignore pointer))
  (show-cursor 0))
#++
(defmethod glop2-backend:set-cursor ((win platform-window-win32)
                                     (cursor win32-cursor))
  (set-cursor cursor (hcursor cursor)))

(defmethod glop2:get-cursor-position-using-backend ((application win32-backend-connection) &key pointer)
  (declare (ignore pointer))
  (get-physical-cursor-pos))

(defmethod glop2:get-cursor-position ((win platform-window-win32) &key pointer)
  (declare (ignore pointer))
  (glop2:get-cursor-position-using-backend win))

(defmethod glop2:set-cursor-position-using-backend ((application win32-backend-connection) x y &key pointer)
  (declare (ignore pointer))
  (set-physical-cursor-pos x y))

(defmethod glop2:get-cursor-position ((win platform-window-win32) &key pointer)
  (declare (ignore pointer))
  (glop2:get-cursor-position-using-backend win))

#++
(defmethod glop2:%next-event ((win platform-window-win32) &key blocking)
  (let ((evt (next-event win (id win) blocking)))
    (setf %event% nil)
    evt))


(defmethod %swap-interval ((win platform-window-win32) interval)
  ;; don't check/modify win32-window-swap-interval since we
  ;; might be emulating it with dwm
  (unless (glop2:swap-interval-tear win)
    (setf interval (abs interval)))
  (if (and (cffi:pointerp (glop2:swap-interval-function win))
           (not (cffi:null-pointer-p (swap-interval-function win))))
      (cffi:foreign-funcall-pointer (swap-interval-function win) () :int
                                    interval :int)))

(defun %dwm-composition-changed (win)
  (setf (slot-value (window-backend-connection win) 'dwm-active)
        (dwm-is-composition-enabled))
  (if (dwm-active win)
      (%swap-interval win 0)
      (%swap-interval win (swap-interval win))))

(defun %init-dwm (win)
  (let ((ver (get-version))
        (dwm t))
    (cond
      ((< ver 6.0) ;; no dwm at all
       (setf dwm nil))
      ((< ver 6.2) ;; vista-win7, see if dwm is active
       (setf dwm (dwm-is-composition-enabled))))
    (setf (slot-value (window-backend-connection win) 'dwm-active) dwm)
    dwm))


#++
(defmethod %init-swap-interval ((win platform-window-win32))
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

(defmethod (setf swap-interval) (interval (win platform-window-win32))
  (setf (slot-value win 'swap-interval) interval)
  (unless (dwm-active win)
    (%swap-interval win interval)))


(defmethod glop2:raise-window ((w platform-window-win32))
  (glop2/ffi-win32:bring-window-to-top w))

(defmethod glop2:lower-window ((w platform-window-win32))
  (glop2/ffi-win32:set-window-pos w glop2/ffi-win32:+hwnd-bottom+
                                  0 0 0 0
                                  '(:swp-no-move :swp-no-size)))


(defclass platform-pixmap-win32 (dc-mixin)
  ((bmp :reader bmp :initarg :bmp)
   (width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (old :reader old :initarg :old)))

(defmethod glop2::create-pixmap ((bc win32-backend-connection) w h)
  (let* ((dc (glop2/ffi-win32:get-dc (cffi:null-pointer)))
         (cdc (create-compatible-dc dc))
         (bmp (glop2/ffi-win32-gdi32:create-compatible-bitmap dc w h))
         (old (select-object cdc bmp)))
    (unless (cffi:null-pointer-p dc)
      (unwind-protect
           (make-instance
            'platform-pixmap-win32
            :bmp bmp
            :width w
            :height h
            :dc cdc
            :old old)
        (glop2/ffi-win32:release-dc (cffi:null-pointer) dc)))))

(defmethod glop2:create-pixmap ((pw platform-window-win32) w h)
  (let* ((dc (dc pw))
         (cdc (create-compatible-dc dc))
         (bmp (glop2/ffi-win32-gdi32:create-compatible-bitmap dc w h))
         (old (select-object cdc bmp)))
    (unless (cffi:null-pointer-p dc)
      (make-instance
       'platform-pixmap-win32
       :bmp bmp
       :width w
       :height h
       :dc cdc
       :old old))))

(defmethod glop2:free-pixmap (wbc (pm platform-pixmap-win32))
  (when (old pm)
    (select-object (dc pm) (shiftf (slot-value pm 'old) nil)))
  (when (bmp pm)
    (delete-object (shiftf (slot-value pm 'bmp) nil)))
  (when (dc pm)
    (delete-object (shiftf (slot-value pm 'dc) nil))))

#++
(defmethod glop2:free-pixmap ((bc win32-backend-connection) pixmap)
  (glop2/ffi-win32-gdi32:delete-object pixmap))

(defmethod glop2:flush ((bc win32-backend-connection))
  (glop2/ffi-win32-gdi32:gdi-flush))

(defmethod glop2:get-cursor-pos ((bc win32-backend-connection))
  (glop2/ffi-win32:get-cursor-pos))

(defmethod glop2::%blit-pointer ((win platform-window-win32)
                                 pointer x y w h)
  (let* ((dc (dc win))
         (cdc (create-compatible-dc dc))
         (bmp (create-compatible-bitmap dc w h)))
    (set-di-bits cdc bmp pointer w h)
    (with-selected-object (cdc bmp)
      (bit-blt dc x y w h cdc 0 0 :src-copy))
    (delete-object bmp)
    (delete-object cdc)))

(defmethod glop2::blit ((from dc-mixin) fx fy w h
                        (to dc-mixin) tx ty)
  (let ((fdc (dc from))
        (tdc (dc to)))
    (format *debug-io* "blit ~sx~s from ~s,~s to ~s,~s~%" w h fx fy tx ty)
    (bit-blt tdc tx ty w h fdc fx fy :src-copy)))
