;;; common code
(defsystem glop2/base
  :license "MIT"
  :version "0.1.0"
  :description "Direct FFI bindings for xlib/win32/osx/etc window creation/management + input handling, and OpenGL etc context management"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:alexandria :trivial-garbage :static-vectors)
  :components
  ((:module "base"
    :serial t
    :components ((:file "package")
                 (:file "base")
                 (:file "application")
                 (:file "window")
                 (:file "events")
                 (:file "backend")
                 ))))

;;; ffi bindings

;; splitting out win32 ffi by dll, hopefully not too verbose
(asdf:defsystem glop2/ffi-win32
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :description "Win32 ffi for kernel32.dll, user32.dll"
  :depends-on (:cffi)
  :components
  ((:module "ffi/win32"
    :serial t
    :components ((:file "package-ffi")
                 (:file "win32")))))

(asdf:defsystem glop2/ffi-win32-dwm
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :description "Win32 Dwmapi.dll ffi"
  :depends-on (:cffi)
  :components
  ((:module "ffi/win32/dwm"
    :serial t
    :components ((:file "package")
                 (:file "dwm")))))

(asdf:defsystem glop2/ffi-win32-gdi32
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :description "Win32 gdi32.dll ffi"
  :depends-on (:cffi :glop2/ffi-win32)
  :components
  ((:module "ffi/win32/gdi32"
    :serial t
    :components ((:file "package")
                 (:file "gdi32")))))

(asdf:defsystem glop2/ffi-win32-wgl
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :description "Win32 wl.dll ffi"
  :depends-on (:cffi :glop2/ffi-win32)
  :components
  ((:module "ffi/win32/wgl"
    :serial t
    :components ((:file "package")
                 (:file "wgl")))))

;; X ffi are separated by xlib/xcb, so multiple related shared libs
(asdf:defsystem glop2/ffi-xlib
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:cffi)
  :components
  ((:module "ffi/xlib"
    :serial t
    :components ())))
#++
(asdf:defsystem glop2/ffi-xcb
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:cffi)
  :components
  ((:module "ffi/xcb"
    :serial t
    :components ())))

#++
(asdf:defsystem glop2/ffi-osx
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:cffi)
  :components
  ((:module "ffi/osx"
    :serial t
    :components ())))


;; using external libs for egl, vulkan, drm ffi bindings for now



;;; windowing system backends

(asdf:defsystem glop2/backend-win32
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :description "Glop2 win32 backend"
  :depends-on (:glop2/base :glop2/ffi-win32
               :glop2/ffi-win32-dwm
                           :glop2/ffi-win32-gdi32)
  :components
  ((:module "backend/win32"
    :serial t
    :components ((:file "package")
                 (:file "wndproc")
                 (:file "win32")))))

(asdf:defsystem glop2/backend-xlib
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :description "Glop2 x11/xlib backend"
  :depends-on (:glop2/base :glop2/ffi-xlib)
  :components
  ((:module "backend/xlib"
    :serial t
    :components ())))
#++
(asdf:defsystem glop2/backend-xcb
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :description "Glop2 x11/xcb backend"
  :depends-on (:glop2/base :glop2/ffi-xcb)
  :components
  ((:module "backend/xcb"
    :serial t
    :components ())))


(asdf:defsystem glop2/backend-drm
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :description "Glop2 drm backend"
  :depends-on (:glop2/base
               :cl-drm
               :cl-gbm
               ;; fixme: decide on proper separation between drm and egl
               ;; and possibly remove this
               :cl-egl
               :dbus
               :osicat
               :cl-libinput
               :bordeaux-threads
               :atomics
               :chanl)
  :components
  ((:module "backend/drm"
    :serial t
    :components ((:file "package")
                 (:file "console")
                 (:file "udev")
                 (:file "input")
                 (:file "epoll")
                 (:file "event-thread")
                 (:file "drm")
                 (:file "session")))))


#++
(asdf:defsystem glop2/backend-osx
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :description "Glop2 OSX backend"
  :depends-on (:glop2/base :glop2/ffi-osx)
  :components
  ((:module "backend/osx"
    :serial t
    :components ())))


;;; rendering lib layers
(asdf:defsystem glop2/backend-wgl
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  ;; wgl is GL only, so use cl-opengl for gl bindings (used to check
  ;; context versions, etc)
  :depends-on (:glop2/base
               :glop2/ffi-win32-wgl
               :glop2/ffi-win32-gdi32
               :cl-opengl)
  :components
  ((:module "backend/wgl"
    :serial t
    :components ())))

;;; don't use this directly, use API specific version below
(asdf:defsystem glop2/backend-glx
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  ;; no ffi lib, depends on xlib window layer for os specific calls?
  :depends-on (:glop2/base)
  :components
  ((:module "backend/glx"
    :serial t
    :components ())))

(asdf:defsystem glop2/backend-glx-gl
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:glop2/backend-glx :cl-opengl)
  :components
  ((:module "backend/glx"
    :serial t
    :components ())))

(asdf:defsystem glop2/backend-glx-es
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:glop2/backend-glx :cl-opengl/es2)
  :components
  ((:module "backend/glx"
    :serial t
    :components ())))

;;; don't use this directly, use one of the rendering-api specific
;;; versions below?
(asdf:defsystem glop2/backend-egl
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:glop2/base :cl-egl)
  :components
  ((:module "backend/egl"
    :serial t
    :components ((:file "package")
                 (:file "egl")))))

;; can't load gl and gles versions of cl-opengl at the same time on
;; most lisps, so there are separate systems for each
(asdf:defsystem glop2/backend-egl-gl
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  ;; depends on platform backend for os specific calls
  :depends-on (:glop2/backend-egl :cl-opengl)
  :components
  ((:module "backend/egl"
    :serial t
    :components ())))

(asdf:defsystem glop2/backend-egl-es
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  ;; depends on platform backend for os specific calls
  :depends-on (:glop2/backend-egl :cl-opengl/es2)
  :components
  ((:module "backend/egl"
    :serial t
    :components ())))

#++
(asdf:defsystem glop2/backend-vulkan
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:glop2/base :cl-vulkan)
  :components
  ((:module "backend/vk"
    :serial t
    :components ())))

#++
(asdf:defsystem glop2/backend-metal
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:glop2/base :glop2/ffi-osx)
  :components
  ((:module "backend/metal"
    :serial t
    :components ())))

#++
(asdf:defsystem glop2/backend-d3d
  :license "MIT"
  :version "0.1.0"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:glop2/base :glop2/ffi-win32)
  :components
  ((:module "backend/d3d"
    :serial t
    :components ())))

;;; combined layers

;; default, picks a 'common' combination depending on OS
(asdf:defsystem glop2
  :license "MIT"
  :version "0.1.0"
  :description "Direct FFI bindings for OpenGL window and context management"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:glop2/base
               ;; on unix, default to xlib+glx+GL
               (:feature (:and unix (:not darwin)) :glop2/backend-xlib)
               (:feature (:and unix (:not darwin)) :glop2/backend-glx-gl)
               ;; on windows, wgl+win32+GL
               (:feature :win32 :glop2/backend-win32)
               (:feature :win32 :glop2/backend-wgl)
               ;; on darwin, osx+?+GL
               #++(:feature windows :glop2/backend-osx)
               ;; ??
               )
  :components
  ((:module "glop2"
    :serial t
    :components ())))

;; not sure if there will be separate systems for other combinations
;; or just let users load the parts they need?
(asdf:defsystem glop2/drm+egl
  :license "MIT"
  :version "0.1.0"
  :description "Direct FFI bindings for OpenGL window and context management"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (:glop2/base
               ;; probably unix only, but let users try anyway
               :glop2/backend-drm
               :glop2/backend-egl)
  :components
  ((:module "backend/egl"
    :serial t
    :components ((:file "egl-drm")))
   (:module "glop2"
    :serial t
    :components ())))



