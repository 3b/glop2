(in-package glop2/backend-egl)

(defmethod glop2::open-context ((cb context-backend-egl)
                                (wb glop2/backend-drm::platform-window-drm)
                                attribs)
  (format t "todo open-context~%"))

#++
(defmethod create-gl-context ((win drm-window)
                              &key (make-current t)
                                major minor
                                forward-compat debug
                                profile share)
  #++
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
                 (drm-window-dc win) attrs :share (if share
                                                        (egl-context-ctx share)
                                                        (cffi:null-pointer))))
              (egl:create-context (drm-window-dc win))))
    (unless (egl-context-ctx ctx)
      (format t "Error creating GL context: ~S~%" (glop-drm::get-last-error-string)))
    (when make-current
      (attach-gl-context win ctx))
    (when (and major minor)
      (egl:correct-context? major minor))
    (let ((e (egl::get-error)))
      (unless (zerop e)
        (warn "got gl error ~s during context creation?" e)))
    (%init-swap-interval win)
    ctx))

#++
(defmethod attach-gl-context ((win drm-window) (ctx egl-context))
  #++
  (setf (window-gl-context win) ctx)
  #++
  (egl:make-current (drm-window-dc win) (egl-context-ctx ctx)))

#++
(defmethod choose-pixel-format ((win drm-window) &rest r
                                &key &allow-other-keys)
  #++
  (apply #'glop-drm:choose-pixel-format (drm-window-dc win) r))
