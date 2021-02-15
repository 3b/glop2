(in-package #:glop2/ffi-win32-dwm)

(cffi:define-foreign-library dwm
  (:windows "Dwmapi.dll"))

(use-foreign-library dwm)

(defcfun ("DwmFlush" dwm-flush) :int)

(defcfun ("DwmIsCompositionEnabled" %dwm-is-composition-enabled) :int32
  (enabled (:pointer bool)))

(defun dwm-is-composition-enabled ()
  (with-foreign-object (p 'bool)
    (let ((hr (%dwm-is-composition-enabled p)))
      (if (zerop hr)
          (not (zerop (mem-ref p 'bool)))
          (error "dwm-is-composition-enabled failed 0x~x~%" hr)))))
