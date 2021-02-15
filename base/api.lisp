(in-package #:glop2)

(defgeneric hide-cursor (window))
(defgeneric show-cursor (window))

;;; cursor list (most are aliases on osx & windows, hopefully map reasonably)
;;; (may manually define some more of them, or allow loading from a
;;;  theme of some sort at some point?)
;; :arrow
;; :default (= :arrow)
;; :wait
;; :wait-arrow
;; :cross ;; cross-hair
;; :hand ;; (pointing hand, for text links)
;; :size-all
;; :size-n
;; :size-e
;; :size-s
;; :size-w
;; :size-ew
;; :size-ns
;; :size-nesw
;; :size-nwse
;; :i-beam ;; text
;; :i-beam-vertical
;; :no ;; circle with line through
;; :no-arrow
;; :up-arrow
;; :help ;; question mark or question-mark arrow
;; :cell ;; thick cross for spreadsheet cell
;; :grab ;; open hand if available
;; :move ;; closed hand if available
;; :move-horizontal ;; closed hand or left-right arrow
;; :move-vertical ;; closed hand or up-down arrows

;;; glfw cursor list https://www.glfw.org/docs/latest/group__shapes.html
;; arrow
;; i-beam
;; crosshair
;; hand
;; h-resize
;; v-resize

;;; sdl2 cursor list https://wiki.libsdl.org/SDL_CreateSystemCursor
;; arrow
;; i-beam
;; wait
;; crosshair
;; wait-arrow
;; size-nwse
;; size-nesw
;; size-we
;; size-ns
;; size-all
;; no
;; hand

;;; windows cursor list
;;; https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-loadcursora
;; arrow
;; wait
;; app-starting (wait-arrow)
;; help (?-arrow)
;; cross
;; hand
;; size-all
;; size-ns
;; size-we
;; size-nesw
;; size-nwse
;; i-beam
;; no
;; up-arrow

;;; osx cursor list https://developer.apple.com/documentation/appkit/nscursor
;; arrow
;; i-beam
;; crosshair
;; pointing-hand
;; resize-left-right
;; resize-up-down
;; closed-hand
;; open-hand
;; resize-left
;; resize-right
;; resize-up
;; resize-down
;; disappearing-item
;; i-beam-cursor-for-vertical-layout
;; operation-not-allows (arrow+no)
;; drag-link
;; drag-copy
;; context-menu


;;; xdg cursor list:
;;;  https://www.freedesktop.org/wiki/Specifications/cursor-spec/
;; default = normal cursor, used on buttons, scrollbars, etc. = up-left arrow
;; text = used over text, = i-beam
;; pointer = text links = pointing hand
;; help = click for help = arrow with ? mark
;; busy ('progress') = busy but still accepting input = pointer + spinner or hourglass
;; wait = blocked = spinner/hourglass/stopwatch
;; copy = drag'n'drop copy = arrow with + sign
;; alias = drag'n'drop alias/link = arrow + curved arrow
;; no-drop = drag'n'drop not allowd = circle with diagonal line through
;; not-allowed = invalid = circle with diagonal line through
;; move ('all-scroll') = moving UI or contents in any direction = 4way arrow
;; ew-move = moving ui or contents horizontally = left/right arrow
;; ns-move = mobing ui or contents vertically = up/down arrow
;; row-resize = horizontal splitter bar cursor = up/down arrow w/line through
;; col-resize = vertical splitter bar cursor = left/right arrow w/line through
;; e-resize = resize edge right = right or left/right arrow
;; ne-resize = resize up/right = up/right or ne/sw diagonal arrow
;; nw-resize = resize up/left = up/left or nw/se diagonal arrow
;; n-resize = resize edge up = up or up/down arrow
;; se-resize = resize down/right = down/right or nw/se diagonal arrow
;; sw-resize = resize down/left = down/left or ne/sw diagonal arrow
;; s-resize = resize edge down = down or up/down arrow
;; w-resize = resize edge left = left or left/right arrow
;; ew-resize = resize left or right = left/right arrow
;; ns-resize = resize up or down = up/down arrow
;; nesw-resize = resize diagonal ne/sw = ne/sw arrow
;; nwse-resize = resize up diagonal nw/se = nw/se arrow
;; vertical-text = used over vertical text = horizontal i-beam
;; cross = precision select = crosshair
;; cell = spreadsheet cell select = thick cross
;; up-arrow = insertion point = up-arrow
;; context-menu = context menu available for item under cursor = arrow + menu
