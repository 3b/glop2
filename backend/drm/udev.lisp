(in-package glop2/backend-drm)

;; minimal udev bindings to use libinput udev backend

(cffi:define-foreign-library libudev
  (:unix (:or "libudev.so.1"
              "libudev.so"))
  (t (:default "libudev")))

(cffi:use-foreign-library libudev)

(cffi:defcfun ("udev_new" udev-new) :pointer)

(cffi:defcfun ("udev_ref" udev-ref) :pointer
  (udev :pointer))

(cffi:defcfun ("udev_unref" udev-unref) :pointer
  (udev :pointer))
