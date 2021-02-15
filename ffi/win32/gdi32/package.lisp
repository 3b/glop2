(defpackage #:glop2/ffi-win32-gdi32
  (:use :cl #:cffi #:glop2/ffi-win32)
  (:export
   #:choose-pixel-format
   #:swap-buffers
   #:pixelformatdescriptor
   #:pixel-format-descriptor
   #:create-compatible-bitmap
   #:delete-object
   #:gdi-flush
   #:create-dib-section
   #:create-compatible-dc
   #:select-object
   #:set-di-bits
   #:bit-blt
   #:with-selected-object))
