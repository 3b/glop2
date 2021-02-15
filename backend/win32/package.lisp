(defpackage :glop2/backend-win32
  (:use #:cl
        #:glop2/ffi-win32
        #:glop2/ffi-win32-dwm
        #:glop2/ffi-win32-gdi32)
  (:import-from #:glop2/ffi-win32
                #:msg-type
                #:unregister-class
                #:destroy-window
                #:set-cursor
                #:destroy-cursor)
  (:import-from #:glop2
                #:platform-window
                #:x #:y #:width #:height #:title #:fullscreen
                #:window
                #:previous-video-mode
                #:context-backend
                )
  (:export
   #:get-system-metrics))
