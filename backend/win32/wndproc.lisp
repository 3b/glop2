(in-package #:glop2/backend-win32)


(defvar *window-id-mapping* (tg:make-weak-hash-table :weakness :value))

;; XXX: this is an ugly hack and should probably be changed
;; We use the %event% var to allow window-proc callback to generate
;; glop2:event objects that can be return from next-event

(defvar %event% nil)

(defconstant +wheel-delta+ 120.0)

(defun next-event (win wnd &optional blocking)
  (declare (ignore win))
  (with-message (msg)
    (let ((r (if blocking
                 (%get-message msg wnd 0 0)
                 (%peek-message msg wnd 0 0 :pm-remove))))
      (unless (minusp r)
        (cond
          ((and blocking (zerop r))
           (return-from next-event (list :quit nil)))
          ((and (not blocking) (zerop r))
           (return-from next-event nil))
          ((eql :wm-quit
                (cffi:foreign-enum-keyword
                 'msg-type
                 (cffi:foreign-slot-value msg '(:struct glop2/ffi-win32::msg)
                                          'glop2/ffi-win32::message)
                 :errorp nil))
           (return-from next-event (list :quit nil)))
          (t
           #++(format t "~s ~a~%" r (cffi:mem-ref msg '(:struct msg)))
           (%translate-message msg)
           (%dispatch-message msg))))))
  #++(when %event% (print %event%))
  (let ((ev (reverse (shiftf %event% nil))))
    ;; we may need to remap hwnd to window instance here to properly handle
    ;; synchronous events during window creation properly, since hwnd
    ;; hasn't been added to mapping yet at that point
    (loop for e in ev
          when (numberp (second e))
            do (setf (second e) (gethash (second e) *window-id-mapping*))
               ;; some other processing here as well for similar reasons
               (let ((w (second e)))
                 (when w
                   (case (car e)
                     (glop2:on-resize
                      (if (%update-geometry-from-window (platform-window w))
                          (progn
                            (setf (third e) (x w))
                            (setf (fourth e) (y w))
                            (setf (fifth e) (width w))
                            (setf (sixth e) (height w)))
                          (setf (car e) nil)))))))
    (setf ev (remove nil ev :key 'car))
    (when ev
      (setf (glop2::pushed-events glop2::*application*)
            (append (glop2::pushed-events glop2::*application*) ev))))
  %event%)

(let ((last-x 0)
      (last-y 0))
  (cffi:defcallback window-proc :long
    ((wnd hwnd) (msg :uint) (w-param wparam) (l-param lparam))
    (let ((msg-type (cffi:foreign-enum-keyword 'msg-type msg :errorp nil))
          (%window% (gethash (cffi:pointer-address wnd) *window-id-mapping*)))
      (labels ((event (ev &rest args)
                 #++(when %event%
                      (format t "dropped event ~s @ ~s?~%" %event% ev))
                 (if %window%
                     (push (list* ev %window% args) %event%)
                     (push (list* ev (cffi::pointer-address wnd) args) %event%)))
               (resize-event ()
                 ;; size filled in later if we don't know window yet
                 (cond
                   (%window%
                    (when (%update-geometry-from-window
                           (platform-window %window%))
                      (event 'glop2:on-resize
                             (x %window%)
                             (y %window%)
                             (width %window%)
                             (height %window%))))
                   (t
                    (event 'glop2:on-resize nil nil nil nil)))))
        (case msg-type
          (:wm-close
           (event 'glop2:on-close)
           (return-from window-proc 0))
          (:wm-quit
           (event :quit)
           (return-from window-proc 0))
          (:wm-destroy
           ;; post-quit-message here sounds wrong, don't want to exit
           ;; main loop if only 1 window closed and others are open...
           (if %window%
               (progn
                 (remhash %window% *window-id-mapping*)
                 (remhash %window% (glop2::active-windows glop2::*application*))
                 (when (zerop (hash-table-count (glop2::active-windows
                                                 glop2::*application*)))
                   (format t "destroyed last window~%")
                   (glop2/ffi-win32::%post-quit-message 0)))
               (progn
                 (format t "got :wm-destroy for unknown window ~s?" wnd)
                 (%post-quit-message 0)))
           (return-from window-proc 0))
          (:wm-mouse-move
           (let ((low (low-word l-param))
                 (high (high-word l-param)))
             (when (or (/= low last-x) (/= high last-y))
               (event 'glop2:on-mouse-motion
                      low high         ;; x,y
                      (- low last-x)   ;; dx
                      (- high last-y)) ;; dy
               (setf last-x low last-y high))
             (return-from window-proc 0)))
          (:wm-size
           (resize-event)
           (return-from window-proc 0))
          (:wm-move
           (resize-event)
           (return-from window-proc 0))
          (:wm-enter-size-move
           (event 'glop2:on-start-resize)
           (return-from window-proc 0))
          (:wm-exit-size-move
           (event 'glop2:on-end-resize)
           (resize-event)
           (return-from window-proc 0))
          (:wm-paint ;; XXX: we need to call defaut windowproc too here
           (multiple-value-bind (x y w h)
               (get-update-rect wnd)
             (event 'glop2:on-paint x y w h)))
          (:wm-lbutton-down
           (set-capture wnd)
           (event 'glop2:on-button t 1)
           (return-from window-proc 0))
          (:wm-lbutton-up
           (release-capture)
           (event 'glop2:on-button nil 1)
           (return-from window-proc 0))
          (:wm-rbutton-down
           (set-capture wnd)
           (event 'glop2:on-button t 3)
           (return-from window-proc 0))
          (:wm-rbutton-up
           (release-capture)
           (event 'glop2:on-button nil 3)
           (return-from window-proc 0))
          (:wm-mbutton-down
           (set-capture wnd)
           (event 'glop2:on-button t 2)
           (return-from window-proc 0))
          (:wm-mbutton-up
           (release-capture)
           (event 'glop2:on-button nil 2)
           (return-from window-proc 0))
          (:wm-key-up
           (multiple-value-bind (keysym text) (win32-lookup-key w-param l-param)
             (setf (glop2:key-pressed w-param) nil)
             (event 'glop2:on-key
                    t                   ; key pressed
                    w-param             ; keycode
                    keysym
                    text))
           (return-from window-proc 0))
          (:wm-key-down
           (multiple-value-bind (keysym text) (win32-lookup-key w-param l-param)
             (when (and glop2:*ignore-auto-repeat*
                        (glop2:key-pressed w-param))
               (return-from window-proc 0))
             (setf (glop2:key-pressed w-param) t)
             (event 'glop2:on-key
                    nil                 ; key released
                    w-param             ; keycode
                    keysym
                    text))
           (return-from window-proc 0))
          (:wm-mouse-wheel
           (event 'glop2:on-mouse-wheel 0 (/ (ash w-param -16)
                                             +wheel-delta+))
           (return-from window-proc 0))
          (:wm-show-window
           (event 'glop2:on-visibility (zerop w-param))
           (return-from window-proc 0))
          (:wm-set-focus
           (event 'glop2:on-focus t)
           (return-from window-proc 0))
          (:wm-kill-focus
           (event 'glop2:on-focus nil)
           (return-from window-proc 0))
          (:wm-erase-background
           (return-from window-proc 0))
          (:wm-set-cursor
           ;; not sure if this should be exposed to users or entirely
           ;; internal?
           #++
           (format t "set cursor event ~x ~x ~s~%" w-param l-param
                   (cffi:foreign-enum-keyword
                    'glop2/ffi-win32::hit-test-code
                    (ldb (byte 16 0) l-param)))
           (when (and %window%
                      (= (ldb (byte 16 0) l-param)
                         #.(cffi:foreign-enum-value
                            'glop2/ffi-win32::hit-test-code
                            :ht-client)))
             (%set-cursor (glop2::platform-window %window%))
             (return-from window-proc 1)))
          (:wm-display-change
           (enumerate-displays))
          (:wm-dwm-composition-changed
           ;; assuming if we get this, dwm-is-composition-enabled returns
           ;; meaningful data...
           (%dwm-composition-changed %window%))))
      ;; Pass unhandled messages to default windowproc
      (%def-window-proc wnd msg w-param l-param))))

