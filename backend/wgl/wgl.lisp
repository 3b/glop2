(in-package  #:glop2/backend-wgl)

(defclass context-backend-wgl (context-backend)
  ())
(glop2::add-context-backend 'context-backend-wgl :gl :gles)
(glop2::add-context-default-backend 'context-backend-wgl :gl :gles)


;; this should possbly be a method on backend? (and/or have an option to return
(defun gl-get-proc-address (proc-name)
  (glop-wgl:wgl-get-proc-address proc-name))

(defmethod create-gl-context ((win win32-window)
                              &key (make-current t)
                                major minor
                                forward-compat debug
                                profile share)

  (let ((ctx (make-wgl-context)))
    (setf (wgl-context-ctx ctx)
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
                (glop-wgl:wgl-create-specific-context
                 (win32-window-dc win) attrs :share (if share
                                                        (wgl-context-ctx share)
                                                        (cffi:null-pointer))))
              (glop-wgl:wgl-create-context (win32-window-dc win))))
    (unless (wgl-context-ctx ctx)
      (format t "Error creating GL context: ~S~%" (glop-win32::get-last-error-string)))
    (when make-current
      (attach-gl-context win ctx))
    (when (and major minor)
      (glop-wgl:correct-context? major minor))
    (let ((e (glop-wgl::get-error)))
      (unless (zerop e)
        (warn "got gl error ~s during context creation?" e)))
    (%init-swap-interval win)
    ctx))

(defmethod destroy-gl-context ((ctx wgl-context))
  (detach-gl-context ctx)
  (glop-wgl:wgl-delete-context (wgl-context-ctx ctx)))

(defmethod attach-gl-context ((win win32-window) (ctx wgl-context))
  (setf (window-gl-context win) ctx)
  (glop-wgl:wgl-make-current (win32-window-dc win) (wgl-context-ctx ctx)))

(defmethod detach-gl-context ((ctx wgl-context))
  (glop-wgl::wgl-make-current (cffi:null-pointer) (cffi:null-pointer)))

(defmethod choose-pixel-format (win &rest r &key &allow-other-keys)
  (apply #'glop-win32:choose-pixel-format (win32-window-dc win) r))

