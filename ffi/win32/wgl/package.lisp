(defpackage #:glop2/ffi-win32-wgl
  (:use :cl #:cffi :glop2/ffi-win32)
  (:export
   #:wgl-get-proc-address
   #:wgl-delete-context
   #:wgl-get-current-dc
   #:wgl-make-current
   #:wgl-create-specific-context
   #:wgl-create-context
   #:wgl-context
   #:hglrc))
