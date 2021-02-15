;; WGL bindings
(in-package #:glop2/ffi-win32-wgl)

(defcenum (wgl-context-attributes :unsigned-int)
  (:major-version #x2091)
  (:minor-version #x2092)
  (:layer-planes #x2093)
  (:flags #x2094)
  (:profile-mask #x9126)
  (:core-profile-bit #x00000001)
  (:compatibility-profile-bit #x00000002))

(defbitfield (wgl-context-attribute-flags :unsigned-int)
  (:debug-bit #x00000001)
  (:forward-compatible-bit #x00000002))

(defctype hglrc handle)

(defstruct wgl-context
  ctx)

(defcfun ("wglCreateContext" wgl-create-context) hglrc
  (dc hdc))

(defun wgl-create-specific-context (hdc context-attribs
                                    &key (share (cffi:null-pointer)))
  (with-foreign-object (atts :int (1+ (length context-attribs)))
    (loop
      for i below (length context-attribs)
      for attr in context-attribs do
        (setf (mem-aref atts :int i)
              (typecase attr
                (keyword (foreign-enum-value 'wgl-context-attributes attr))
                (list (foreign-bitfield-value 'wgl-context-attribute-flags attr))
                (t attr))))
    (setf (mem-aref atts :int (length context-attribs)) 0)
    ;; we need a fake gl context to be able to use wgl-get-proc-address
    ;; see http://www.opengl.org/wiki/Creating_an_OpenGL_Context#Proper_Context_Creation
    (let ((tmp-ctx  (wgl-create-context hdc)))
      (wgl-make-current hdc tmp-ctx)
      (let ((ptr (wgl-get-proc-address "wglCreateContextAttribsARB")))
        ;; remove out temporary context
        (wgl-make-current (cffi:null-pointer) (cffi:null-pointer))
        (wgl-delete-context tmp-ctx)
        (when (null-pointer-p ptr)
          (error "wglCreateContextAttribsARB unavailable"))
        (let ((ctx (cffi:foreign-funcall-pointer ptr ()
                                                 :pointer hdc
                                                 :pointer share
                                                 (:pointer :int) atts
                                                 :pointer)))
          (when (null-pointer-p ctx)
            (error "Unable to create context"))
          ctx)))))

(defcfun ("wglMakeCurrent" wgl-make-current) bool
  (dc hdc) (rc hglrc))

(defcfun ("wglGetCurrentDC" wgl-get-current-dc) hdc)

(defcfun ("wglDeleteContext" wgl-delete-context) bool
  (rc hglrc))

(defcfun ("wglGetProcAddress" wgl-get-proc-address) :pointer
  (proc-name :string))



