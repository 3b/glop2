(in-package glop2/backend-egl)


(defclass context-backend-egl (glop2::context-backend)
  ())
(glop2::add-context-backend 'context-backend-egl :gl :gles)
(glop2::add-context-default-backend 'context-backend-egl :gl :gles)


;; this should possbly be a method on backend? (and/or have an option to return
(defun gl-get-proc-address (proc-name)
  (egl::get-proc-address proc-name))

#++
(defmethod destroy-gl-context ((ctx egl-context))
  (detach-gl-context ctx)
  (egl:delete-context (egl-context-ctx ctx)))

#++
(defmethod detach-gl-context ((ctx egl-context))
  (egl:make-current (cffi:null-pointer) (cffi:null-pointer)))

