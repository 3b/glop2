(in-package glop2/backend-egl)

(defmethod create-gl-context ((win win32-window)
                              &key (make-current t)
                                major minor
                                forward-compat debug
                                profile share)

  (let ((ctx (make-egl-context)))
    (setf (egl-context-ctx ctx)
          (if (or major minor forward-compat debug profile share)
              (let ((attrs))
                (flet ((f (k v)
                         (push v attrs)
                         (push k attrs)))
                  (when major (f :major major))
                  (when minor (f :minor minor))
                  (when profile
                    (f :profile-mask
                       (case profile
                         (:core (push :core-profile-bit attrs))
                         (:compat (push :compatibility-profile-bit attrs)))))
                  (when (or forward-compat debug)
                    (let ((flags '()))
                      (when forward-compat (push :forward-compatible-bit flags))
                      (when debug (push :debug-bit flags))
                      (f :flags flags))))
                (format t "attrs ~s~%" attrs)
                (egl:create-specific-context
                 (win32-window-dc win) attrs :share (if share
                                                        (egl-context-ctx share)
                                                        (cffi:null-pointer))))
              (egl:create-context (win32-window-dc win))))
    (unless (egl-context-ctx ctx)
      (format t "Error creating GL context: ~S~%" (glop-win32::get-last-error-string)))
    (when make-current
      (attach-gl-context win ctx))
    (when (and major minor)
      (egl:correct-context? major minor))
    (let ((e (egl::get-error)))
      (unless (zerop e)
        (warn "got gl error ~s during context creation?" e)))
    (%init-swap-interval win)
    ctx))

(defmethod attach-gl-context ((win win32-window) (ctx egl-context))
  (setf (window-gl-context win) ctx)
  (egl:make-current (win32-window-dc win) (egl-context-ctx ctx)))

(defmethod choose-pixel-format ((win win32-window) &rest r
                                &key &allow-other-keys)
  (apply #'glop-win32:choose-pixel-format (win32-window-dc win) r))
