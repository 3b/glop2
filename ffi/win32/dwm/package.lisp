(defpackage :glop2/ffi-win32-dwm
  (:use #:cl #:cffi #:glop2/ffi-win32)
  (:export
   #:dwm-flush
   #:dwm-is-composition-enabled
   #:%dwm-is-composition-enabled))
