(in-package glop2/backend-drm)

#++ ;; not used with udev backend?
(defvar *libinput-device-types*
  ;; we look for following device caps when deciding which devices to
  ;; open ;; todo: make this configurable
  '(libinput:device-cap-gesture ;; touchpad gestures
    libinput:device-cap-keyboard
    libinput:device-cap-pointer
    ;libinput:device-cap-tablet-pad
    ;libinput:device-cap-tablet-tool
    libinput:device-cap-touch
    ;libinput:device-cap-switch
    ))

(defclass input ()
  ((tty-fd :initform nil :accessor tty-fd)
   (libinput-fd :initform nil :accessor libinput-fd)
   (context :initform nil :accessor context)
   (old-keyboard-mode :initform nil :accessor old-keyboard-mode)
   (udev :initform nil :accessor udev)))

(defun ioctl/l (fd req arg)
  (nix:ioctl fd req (cffi:make-pointer arg)))


#++
(cffi:defcallback libinput-log-handler ((input :pointer)
                                        (priority libinput:log-priority)
                                        ()))

(defvar *bus* nil)
(cffi:defcallback open-restricted :int
    ((path :string) (flags :int) (user-data :pointer))
  (declare (ignorable flags user-data))
  (format t "Called open-restricted path ~s~%" path)
  (let ((fd (take-device path :bus (or *bus* (application-bus)))))
    (format t "File descriptor ~A~%" fd)
    (when (< fd 0)
      (warn "Failed to open ~A" path))
    fd))

(cffi:defcallback close-restricted :void ((fd :int) (user-data :pointer))
  (declare (ignorable user-data))
  (release-device fd :bus (or *bus* (application-bus))))

(defun make-libinput-interface ()
  (let ((interface (cffi:foreign-alloc
                    '(:struct libinput::libinput-interface))))
    (cffi:with-foreign-slots ((libinput::open-restricted
                               libinput::close-restricted)
                              interface
                              (:struct libinput::libinput-interface))
      (setf libinput::open-restricted (cffi:callback open-restricted))
      (setf libinput::close-restricted (cffi:callback close-restricted)))

    interface))


(defun init-input-libinput (drm)
  (declare (optimize debug)
           (notinline nix:ioctl ioctl/l))
  (let* ((tty (session-property "TTY" :bus (bus drm)))
         (seat (session-property "Seat" :bus (bus drm)))
         (input (make-instance 'input))
         (ok nil)
         (*bus* (bus drm)))
    (unless seat
      (error "no seat? tty=~s" tty))
    (when tty
      (setf tty (format nil "/dev/~a" tty)))
    (unless (and tty (probe-file tty))
      (setf tty "/dev/tty"))
    (format t "open tty ~s~%" tty)
    (unwind-protect
         (progn
           (setf (tty-fd input)
                 (nix:open tty (logior nix:o-rdwr nix:o-noctty)))
           (format t " -> ~s~%" (tty-fd input))
          ;; grab input from tty
           (handler-case
               (ioctl/l (tty-fd input) +kdskbmute+ 1)
             (nix:posix-error ()
               ;; xorg also sets mode to RAW before setting to off?
               ;; https://lists.freedesktop.org/archives/xorg-devel/2012-November/034459.html
               (cffi:with-foreign-object (old-mode :int)
                 (nix:ioctl (tty-fd input) +kdgkbmode+ old-mode)
                 (setf (old-keyboard-mode input) (cffi:mem-ref old-mode :int)))
               (minusp (ioctl/l (tty-fd input) +kdskbmode+ +k-off+))
               #++(error "failed to disable TTY input ~s"
                      (nix:strerror (nix:get-errno)))))
           ;; set console to graphics
           (ioctl/l (tty-fd input) +kdsetmode+ +kd-graphics+)
           ;; initialise libinput
           (format t "open libinput udev context on seat ~s~%" seat)
           (setf (udev input) (udev-new))
           (setf (context input) (libinput:udev-create-context
                                  (make-libinput-interface)
                                  (cffi:null-pointer)
                                  (udev input)))
           (format sb-sys:*stdout* "set log priority from ~s"
                   (libinput:log-get-priority (context input)))
           (libinput:log-set-priority (context input)
                                      :debug)
           (format sb-sys:*stdout* " to ~s~%"
                   (libinput:log-get-priority (context input)))
           (format sb-sys:*stderr* "test stderr~%")
           (libinput:udev-assign-seat (context input) (car seat))
           (libinput:log-set-priority (context input)
                                      :debug)
           (setf (libinput-fd input) (libinput:get-fd (context input)))
           ;; find devices
           #++ ;;(don't need to do this for udev backend, it just
               ;;grabs all devices)
           (flet ((try-caps (dev)
                    (loop
                      for cap in *libinput-device-types*
                        thereis (libinput:device-has-capability dev cap))))
            (loop for path in (directory "/dev/input/event*")
                  for device = (libinput:path-add-device (context input)
                                                         (namestring path))
                  unless (cffi:null-pointer-p device)
                    if (try-caps device)
                      do (format t "add device ~s~%" path)
                  else do (libinput:path-remove-device device)))
           ;; call dispatch once do we can get device add events
           ;; before starting the dispatch loop if needed
           (libinput:dispatch (context input))
           (setf ok t))
      (unless OK
        (destroy-input (shiftf input nil))))
    input))

(defun destroy-input (input)
  (when (tty-fd input)
    (if (old-keyboard-mode input)
      (ioctl/l (tty-fd input) +kdskbmode+ (old-keyboard-mode input))
      (ioctl/l (tty-fd input) +kdskbmute+ 0))
    (ioctl/l (tty-fd input) +kdsetmode+ +kd-text+)
    (nix:close (shiftf (tty-fd input) nil)))
  (when (udev input)
    (udev-unref (shiftf (udev input) nil)))
  ;; fixme: free foreign alloc from make-libinput-interface
  (when (context input)
    (libinput:unref (shiftf (context input) nil))))

(defparameter *dispatch* t)
#++ (setf *dispatch* t)

(defvar *ev-types*
  (alexandria:plist-hash-table
   '(0 :none
     1 :device-added
     2 :device-removed
     300 :keyboard-key
     400 :pointer-motion
     401 :pointer-motion-absolute
     402 :pointer-button
     403 :pointer-axis
     500 :touch-down
     501 :touch-up
     502 :touch-motion
     503 :touch-cancel
     504 :touch-frame
     600 :tablet-tool-axis
     601 :tablet-tool-proximity
     602 :tablet-tool-tip
     603 :tablet-tool-button
     700 :tablet-pad-button
     701 :tablet-pad-ring
     702 :tablet-pad-strip
     800 :gesture-swipe-begin
     801 :gesture-swipe-update
     802 :gesture-swipe-end
     803 :gesture-pinch-begin
     804 :gesture-pinch-update
     805 :gesture-pinch-end)))
(defvar *caps*
  (alexandria:plist-hash-table
   '(0 :keyboard
     1 :pointer
     2 :touch
     3 :tablet-tool
     4 :tablet-pad
     5 :gesture
     6 :switch)))

(defun dispatch-input-events (drm)
  (when *dispatch*
    (let* ((input (input drm))
           (context (when input
                      (context input)))
           (w (win drm))
           ;; todo: refactor to avoid buffering
           (event-list))
      (labels ((tx (us)
                 (format t " @ ~s" (* us 1.0d-6)))
               (keyboard (ev et dev)
                 (declare (ignorable et dev))
                 (tx (libinput:event-keyboard-get-time-usec ev)))
               (pointer (ev et dev)
                 (declare (ignorable et dev))
                 (tx (libinput:event-pointer-get-time-usec ev)))
               (touch (ev et dev)
                 (declare (ignorable et dev))
                 (tx (libinput:event-touch-get-time-usec ev))
                 (let ((xy (or (= et libinput:touch-down)
                               (= et libinput:touch-motion))))
                   (when xy
                     (format t "~&   ~s, ~s @ ~5,3f,~5,3f || ~5,3f,~5,3f~%"
                             (libinput:event-touch-get-slot ev)
                             (libinput:event-touch-get-seat-slot ev)
                             (libinput:event-touch-get-x ev)
                             (libinput:event-touch-get-y ev)
                             (libinput:event-touch-get-x-tranformed ev 800)
                             (libinput:event-touch-get-y-transformed ev 480)))
                   (let ((gev (case et
                                (#.libinput:touch-down
                                 'glop2:on-touch-down)
                                (#.libinput:touch-up
                                 'glop2:on-touch-up)
                                (#.libinput:touch-motion
                                 'glop2:on-touch-motion)
                                (#.libinput:touch-frame
                                 ;; todo: figure out how to assemble a
                                 ;; touch frame into a single event?
                                 'glop2:on-touch-frame))))
                     (cond
                       (xy
                        (push (list
                               gev
                               w
                               (libinput:event-touch-get-slot ev)
                               (libinput:event-touch-get-x-tranformed ev 800)
                               (libinput:event-touch-get-y-transformed ev 480))
                              event-list))
                       ((eql gev 'glop2:on-touch-up)
                        (push (list gev w
                                    (libinput:event-touch-get-slot ev))
                              event-list))
                       (t (push (list gev w)
                                event-list))))))
               (gesture (ev et dev)
                 (declare (ignorable et dev))
                 (tx (libinput:event-gesture-get-time-usec ev)))
               (tablet-tool (ev et dev)
                 (declare (ignorable et dev))
                 (tx (libinput:event-tablet-tool-get-time-usec ev)))
               (tablet-pad (ev et dev)
                 (declare (ignorable et dev))
                 (tx (libinput:event-tablet-pad-get-time-usec ev)))
               (.switch (ev et dev)
                 (declare (ignorable et dev))
                 (tx (libinput:event-switch-get-time-usec ev)))
               (device-notify (ev et dev)
                 (declare (ignorable ev et dev))
                 (ecase et
                   (#.libinput:device-added
                    (format t " added device ~s ~s (~s)~%"
                            (libinput:device-get-name dev)
                            (libinput:device-get-output-name dev)
                            (libinput:device-get-sysname dev))
                    (loop for i below 8
                          when (plusp (libinput:device-has-capability dev i))
                            do (format t " has capability ~s~%"
                                       (gethash i *caps*))))
                   (#.libinput:device-removed
                    (format t " removed device ~s ~s (~s)~%"
                            (libinput:device-get-name dev)
                            (libinput:device-get-output-name dev)
                            (libinput:device-get-sysname dev))))))
        (when context
          #++
          (libinput:dispatch context)
          (loop
            for ev = (libinput:get-event context)
            until (cffi:null-pointer-p ev)
            do (unwind-protect
                    (let* ((et (libinput:event-get-type ev))
                           (dev (libinput:event-get-device ev)))
                      (format t "ev ~s: " (gethash et *ev-types* et))
                      ;; fixme: figure out better way to decide what
                      ;; type of event it is than assuming ranges of
                      ;; 99
                      (cond
                        ((= et 0) ;; type = none
                         (format t "got event of type none?~%"))
                        ((<= 1 et 99)
                         (device-notify
                          (libinput:event-get-device-notify-event ev)
                          et dev))
                        ((<= 300 et 399)
                         (keyboard (libinput:event-get-keyboard-event ev)
                                   et dev))
                        ((<= 400 et 499)
                         (pointer (libinput:event-get-pointer-event ev)
                                  et dev))
                        ((<= 500 et 599)
                         (touch (libinput:event-get-touch-event ev)
                                et dev))
                        ((<= 600 et 699)
                         (tablet-tool (libinput:event-get-tablet-tool-event ev)
                                      et dev))
                        ((<= 700 et 799)
                         (tablet-pad (libinput:event-get-tablet-pad-event ev)
                                     et dev))
                        ((<= 800 et 899)
                         (gesture (libinput:event-get-gesture-event ev)
                                  et dev))
                        ((<= 900 et 999)
                         (.switch (libinput:event-get-switch-event ev)
                                  et dev))
                        (t (format t "unknown event type ~s?~%" et)))))
               (terpri)
               (libinput:event-destroy ev))))
      (nreverse event-list))))
