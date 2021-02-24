(defpackage :glop2-backend
  (:use #:cl)
  (:export
   #:cursors
   #:platform-cursor
   #:backend-connection
   #:monitors
   #:reader
   #:desktop
   #:create-pixmap))

(defpackage :glop2
  (:use #:cl)
  (:import-from #:glop2-backend
                #:cursors)
  (:export
   #:swap-interval-mixin
   #:platform-window
   #:window-x
   #:window-y
   #:open-window
   #:3d-api
   #:set-geometry
   #:close-window
   #:show-winndow
   #:show-window
   #:hide-window
   #:set-window-title
   #:swap-buffers
   #:show-cursor
   #:event
   #:key-event
   #:key-press-event
   #:key-release-event
   #:button-event
   #:button-press-event
   #:button-release-event
   #:mouse-motion-event
   #:expose-event
   #:resize-event
   #:close-event
   #:visibility-event
   #:visibility-obscured-event
   #:visibility-unobscured-event
   #:focus-event
   #:focus-in-event
   #:focus-out-event
   #:child-event
   #:child-created-event
   #:child-destroyed-event
   #:child-reparent-event
   #:child-visibility-event
   #:child-visibility-obscured-event
   #:child-visibility-unobscured-event
   #:child-resize-event
   #:push-event
   #:push-close-event
   #:next-event
   #:dispatch-events
   #:on-event
   #:key-pressed
   #:*ignore-auto-repeat*
   #:swap-interval-function
   #:swap-interval-tear
   #:%next-event
   #:pushed-event
   #:hide-cursor
   #:on-destroy
   #:on-close
   #:on-draw
   #:on-resize
   #:on-mouse-motion
   #:on-button
   #:on-key
   #:on-visibility
   #:on-focus
   #:on-start-resize
   #:on-end-resize
   #:on-mouse-wheel
   #:window
   #:with-application
   #:get-cursor-position
   #:get-cursor-position-using-backend
   #:set-cursort-position
   #:set-cursor-position-using-backend
   #:set-cursor-position
   #:cursors
   #:cursor
   #:screen-width
   #:screen-width-in-millimeters
   #:screen-height
   #:screen-height-in-millimeters
   #:width
   #:height
   #:y
   #:x
   #:lower-window
   #:raise-window
   #:find
   #:primary-monitor
   #:monitor
   #:desktop
   #:width-mm
   #:height-mm
   #:width-in-millimeters
   #:height-in-millimeters
   #:create-pixmap
   #:free-pixmap
   #:flush
   #:get-cursor-pos
   #:event-mask
   #:on-paint
   #:with-blit
   #:on-touch-down
   #:on-touch-up
   #:on-touch-motion
   #:on-touch-frame))


