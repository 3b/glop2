;; Win32  bindings
(in-package #:glop2/ffi-win32)


;; only on windows 32 bit
(defctype wparam :int32)
(defctype lparam :int32)

;; todo:shadow these?
(defctype long :int32)
(defctype long32 :int32)
(defctype long64 :int64)
(defctype longlong :int64)
(defctype word :uint16)
(defctype dword :uint32)
(defctype dword32 :uint32)
(defctype dwordlong :uint64)

(defctype bool :int) ;; XXX: Win32 BOOL isn't used as a boolean (e.g.: see GetMessage)
(defctype boolean :uint8) ;; byte
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (= 8 (foreign-type-size :pointer))
      (defctype int-ptr :int64)
      (defctype int-ptr :int32)))

(define-foreign-type int-or-pointer ()
  ()
  (:actual-type int-ptr)
  (:simple-parser int-or-pointer))

(defmethod translate-to-foreign (value (type int-or-pointer))
  (if (pointerp value)
      (pointer-address value)
      value))

(defmethod expand-to-foreign (value (type int-or-pointer))
  (cond
    ((and (constantp value) (pointerp value))
     (pointer-address value))
    ((and (constantp value) (typep value '(integer 0)))
     value)
    (t
     (alexandria:once-only (value)
       `(if (pointerp ,value)
            (pointer-address ,value)
            ,value)))))

(defctype handle :pointer)
(defctype hwnd handle)
(defctype hdc handle)
(defctype hmenu handle)
(defctype hmodule handle)
(defctype hinstance handle)
(defctype hicon handle)
(defctype hcursor handle)
(defctype hbrush handle)
(defctype hmonitor handle)
(defctype hrgn handle)
(defctype hbitmap handle)

(defcstruct point
  (x :long)
  (y :long))

(defcstruct msg
  (h-wnd hwnd)
  (message :unsigned-int)
  (w-param wparam)
  (l-param lparam)
  (time dword)
  (pt (:struct point)))

(defbitfield wex-style
  (:ws-ex-topmost #x0000008)
  (:ws-ex-app-window #x40000)
  (:ws-ex-window-edge 256))

(defbitfield (wstyle :unsigned-int)
  (:ws-overlapped #x00000000)
  (:ws-popup #x80000000)
  (:ws-child #x40000000)
  (:ws-minimize #x20000000)
  (:ws-visible #x10000000)
  (:ws-disabled #x08000000)
  (:ws-clip-siblings #x04000000)
  (:ws-clip-children #x02000000)
  (:ws-maximize #x01000000)
  (:ws-caption #x00c00000)
  (:ws-border #x00800000)
  (:ws-dialog-frame #x00400000)
  (:ws-vscroll #x00200000)
  (:ws-hscroll #x00100000)
  (:ws-sys-menu #x00080000)
  (:ws-thick-frame #x00040000)
  (:ws-group #x00020000)
  (:ws-tabstop #x00010000)
  (:ws-minimize-box #x00020000)
  (:ws-maximize-box #x00010000)
  (:ws-tiled  #x00000000)
  (:ws-iconic  #x20000000)
  (:ws-sizebox #x00040000)

  (:ws-overlapped-window #xcf0000))

(defcenum gwl-index
  (:gwl-ex-style -20)
  (:gwl-style -16))

(defbitfield class-style-flags
  (:cs-byte-align-client 4096)
  (:cs-byte-align-window 8192)
  (:cs-key-cvt-window 4)
  (:cs-no-key-cvt 256)
  (:cs-class-dc 64)
  (:cs-dbl-clks 8)
  (:cs-global-class 16384)
  (:cs-hredraw 2)
  (:cs-no-close 512)
  (:cs-own-dc 32)
  (:cs-parent-dc 128)
  (:cs-save-bits 2048)
  (:cs-vredraw 1)
  (:cs-ime #x10000)
  (:cs-drop-shadow #x20000))

(defcstruct wndclass
  (style class-style-flags)
  (wndproc :pointer)
  (cls-extra :int)
  (wnd-extra :int)
  (instance hinstance)
  (icon hicon)
  (cursor hcursor)
  (br-background hbrush)
  (menu-name :string)
  (class-name :string))


(defcstruct wndclass-ex
  (size :uint)
  (style class-style-flags)
  (wndproc :pointer)
  (cls-extra :int)
  (wnd-extra :int)
  (instance hinstance)
  (icon hicon)
  (cursor hcursor)
  (br-background hbrush)
  (menu-name :string)
  (class-name :string)
  (small-icon hicon))

(defcenum msg-type
  (:wm-create 1)
  (:wm-destroy 2)
  (:wm-move 3)
  (:wm-size 5)
  (:wm-activate 6)
  (:wm-set-focus 7)
  (:wm-kill-focus 8)
  (:wm-enable #xa)
  (:wm-set-redraw #xb)
  (:wm-set-text #xc)
  (:wm-get-text #xd)
  (:wm-get-text-length #xe)
  (:wm-paint #xf)
  (:wm-close #x10)
  (:wm-quit #x12)
  (:wm-erase-background #x14)
  (:wm-sys-color-change #x15)
  (:wm-show-window #x18)
  (:wm-win-ini-change #x1a)
  (:wm-win-setting-change #x1a)

  (:wm-dev-mode-change #x001b)
  (:wm-activate-app #x001c)
  (:wm-font-change #x001d)
  (:wm-time-change #x001e)
  (:wm-cancel-mode #x001f)
  (:wm-set-cursor #x0020)
  (:wm-mouse-activate #x0021)
  (:wm-child-activate #x0022)
  (:wm-queue-sync #x0023)
  (:wm-get-min-max-info #x24)

  (:wm-paint-icon #x0026)
  (:wm-icon-erase-background #x0027)
  (:wm-next-dialog-control #x0028)
  (:wm-spooler-status #x002a)
  (:wm-draw-item #x002b)
  (:wm-measure-item #x002c)
  (:wm-delete-item #x002d)
  (:wm-vkey-to-item #x002e)
  (:wm-char-to-item #x002f)
  (:wm-set-font #x0030)
  (:wm-get-font #x0031)
  (:wm-set-hotkey #x0032)
  (:wm-get-hotkey #x0033)
  (:wm-query-drag-icon #x0037)
  (:wm-compare-item #x0039)
  (:wm-get-object #x003d)
  (:wm-compacting #x0041)
  (:wm-window-pos-changing #x0046)
  (:wm-window-pos-changed #x0047)
  (:wm-power #x0048)
  (:wm-copy-data #x004a)
  (:wm-cancel-journal #x004b)
  (:wm-notify #x004e)
  (:wm-input-lang-change-request #x0050)
  (:wm-input-lang-change #x0051)
  (:wm-tcard #x0052)
  (:wm-help #x0053)
  (:wm-user-changed #x0054)
  (:wm-notify-format #x0055)
  (:wm-context-menu #x007b)
  (:wm-style-changing #x007c)
  (:wm-style-changed #x007d)
  (:wm-display-change #x007e)
  (:wm-get-icon #x007f)
  (:wm-set-icon #x0080)
  (:wm-nc-create #x0081)
  (:wm-nc-destroy #x0082)
  (:wm-nc-calc-size #x0083)
  (:wm-nc-hit-test #x0084)
  (:wm-nc-paint #x0085)
  (:wm-nc-activate #x0086)
  (:wm-get-dialog-code #x0087)
  (:wm-sync-paint #x0088)
  (:wm-uah-destroy-window #x0090)
  (:wm-uah-draw-menu #x0091)
  (:wm-uah-draw-menu-item #x0092)
  (:wm-uah-init-menu #x0093)
  (:wm-uah-measure-menu-item #x0094)
  (:wm-uah-nc-paint-menu-popup #x0095)
  (:wm-nc-mouse-move #x00a0)
  (:wm-nc-lbutton-down #x00a1)
  (:wm-nc-lbutton-up #x00a2)
  (:wm-nc-lbutton-double-click #x00a3)
  (:wm-nc-rbutton-down #x00a4)
  (:wm-nc-rbutton-up #x00a5)
  (:wm-nc-rbutton-double-click #x00a6)
  (:wm-nc-mbutton-down #x00a7)
  (:wm-nc-mbutton-up #x00a8)
  (:wm-nc-mbutton-double-click #x00a9)
  (:wm-nc-xbutton-down #x00ab)
  (:wm-nc-xbutton-up #x00ac)
  (:wm-nc-xbutton-double-click #x00ad)
  (:wm-input-device-change #x00fe)
  (:wm-input #x00ff)
  (:wm-key-down #x0100)
  (:wm-key-up #x0101)
  (:wm-char #x0102)
  (:wm-dead-char #x0103)
  (:wm-sys-key-down #x0104)
  (:wm-sys-key-up #x0105)
  (:wm-sys-char #x0106)
  (:wm-sys-dead-char #x0107)
  (:wm-ime-start-composition #x010d)
  (:wm-ime-end-composition #x010e)
  (:wm-ime-composition #x010f)
  (:wm-init-dialog #x0110)
  (:wm-command #x0111)
  (:wm-sys-command #x0112)
  (:wm-timer #x0113)
  (:wm-hscroll #x0114)
  (:wm-vscroll #x0115)
  (:wm-init-menu #x0116)
  (:wm-init-menu-popup #x0117)
  (:wm-gesture #x0119)
  (:wm-gesture-notify #x011a)
  (:wm-menu-select #x011f)
  (:wm-menu-char #x0120)
  (:wm-enter-idle #x0121)
  (:wm-menu-rbutton-up #x0122)
  (:wm-menu-drag #x0123)
  (:wm-menu-get-object #x0124)
  (:wm-uninit-menu-popup #x0125)
  (:wm-menu-command #x0126)
  (:wm-change-ui-state #x0127)
  (:wm-update-ui-state #x0128)
  (:wm-query-ui-state #x0129)
  (:wm-ctl-color-msgbox #x0132)
  (:wm-ctl-color-edit #x0133)
  (:wm-ctl-color-listbox #x0134)
  (:wm-ctl-color-btn #x0135)
  (:wm-ctl-color-dlg #x0136)
  (:wm-ctl-color-scrollbar #x0137)
  (:wm-ctl-color-static #x0138)
  (:mn-get-hmenu #x01e1)
  (:wm-mouse-move #x0200)
  (:wm-lbutton-down #x0201)
  (:wm-lbutton-up #x0202)
  (:wm-lbutton-double-click #x0203)
  (:wm-rbutton-down #x0204)
  (:wm-rbutton-up #x0205)
  (:wm-rbutton-double-click #x0206)
  (:wm-mbutton-down #x0207)
  (:wm-mbutton-up #x0208)
  (:wm-mbutton-double-click #x0209)
  (:wm-mouse-wheel #x020a)
  (:wm-xbutton-down #x020b)
  (:wm-xbutton-up #x020c)
  (:wm-xbutton-double-click #x020d)
  (:wm-mouse-hwheel #x020e)
  (:wm-parent-notify #x0210)
  (:wm-enter-menu-loop #x0211)
  (:wm-exit-menu-loop #x0212)
  (:wm-next-menu #x0213)
  (:wm-sizing #x0214)
  (:wm-capture-changed #x0215)
  (:wm-moving #x0216)
  (:wm-power-broadcast #x0218)
  (:wm-device-change #x0219)
  (:wm-mdi-create #x0220)
  (:wm-mdi-destroy #x0221)
  (:wm-mdi-activate #x0222)
  (:wm-mdi-restore #x0223)
  (:wm-mdi-next #x0224)
  (:wm-mdi-maximize #x0225)
  (:wm-mdi-tile #x0226)
  (:wm-mdi-cascade #x0227)
  (:wm-mdi-icon-arrange #x0228)
  (:wm-mdi-get-active #x0229)
  (:wm-mdi-set-menu #x0230)
  (:wm-enter-size-move #x0231)
  (:wm-exit-size-move #x0232)
  (:wm-drop-files #x0233)
  (:wm-mdi-refresh-menu #x0234)
  (:wm-touch #x0240)
  (:wm-ime-set-context #x0281)
  (:wm-ime-notify #x0282)
  (:wm-ime-control #x0283)
  (:wm-ime-composition-full #x0284)
  (:wm-ime-select #x0285)
  (:wm-ime-char #x0286)
  (:wm-ime-request #x0288)
  (:wm-ime-keydown #x0290)
  (:wm-ime-keyup #x0291)
  (:wm-mouse-hover #x02a1)
  (:wm-mouse-leave #x02a3)
  (:wm-nc-mouse-hover #x02a0)
  (:wm-nc-mouse-leave #x02a2)
  (:wm-wt-ssession-change #x02b1)
  (:wm-cut #x0300)
  (:wm-copy #x0301)
  (:wm-paste #x0302)
  (:wm-clear #x0303)
  (:wm-undo #x0304)
  (:wm-render-format #x0305)
  (:wm-render-all-formats #x0306)
  (:wm-destroy-clipboard #x0307)
  (:wm-draw-clipboard #x0308)
  (:wm-paint-clipboard #x0309)
  (:wm-vscroll-clipboard #x030a)
  (:wm-size-clipboard #x030b)
  (:wm-ask-cb-format-name #x030c)
  (:wm-change-cbc-hain #x030d)
  (:wm-hscroll-clipboard #x030e)
  (:wm-query-new-palette #x030f)
  (:wm-palette-is-changing #x0310)
  (:wm-palette-changed #x0311)
  (:wm-hotkey #x0312)
  (:wm-print #x0317)
  (:wm-print-client #x0318)
  (:wm-app-command #x0319)
  (:wm-them-echanged #x031a)
  (:wm-clip-board-update #x031d)
  (:wm-dwm-composition-changed #x031e)
  (:wm-dwm-nc-rendering-changed #x031f)
  (:wm-dwm-colorization-color-changed #x0320)
  (:wm-dwm-window-maximized-change #x0321)
  (:wm-dwm-send-icon-ic-thumbnail #x0323)
  (:wm-dwm-send-icon-ic-live-preview-bitmap #x0326)
  (:wm-get-title-bar-info-ex #x033F))

(defcenum vkey-type
  (:l-button 1)
  :r-button
  :cancel
  :m-button
  :x-button1
  :x-button2
  (:backspace 8)
  :tab
  (:clear #x0c)
  :return
  (:shift #x10)
  :control
  :menu
  :pause
  :capital ;; capslock
  (:kana #x15) ;; also hanguel/hangul
  (:junja #x17)
  (:final #x18)
  (:hanja #x19) ;; also kanji
  (:escape #x1B)
  (:convert #x1C)
  (:no-convert #x1D)
  (:accept #x1E)
  (:mode-change #x1F)
  (:space 32)
  (:page-up 33)
  (:page-down 34)
  (:end 35)
  (:home 36)
  (:left 37)
  (:up 38)
  (:right 39)
  (:down 40)
  (:select 41)
  (:print 42)
  (:execute 43)
  (:snapshot 44) ;; printscreen
  (:insert 45)
  (:delete 46)
  (:help 47)
  (:0 #x30)
  :1
  :2
  :3
  :4
  :5
  :6
  :7
  :8
  :9
  (:a #x41)
  :b
  :c
  :d
  :e
  :f
  :g
  :h
  :i
  :j
  :k
  :l
  :m
  :n
  :o
  :p
  :q
  :r
  :s
  :t
  :u
  :v
  :w
  :x
  :y
  :z
  (:lwin #x5B)
  (:rwin #x5C)
  (:apps #x5D)
  (:sleep #x5F)
  (:numpad0 #x60)
  (:numpad1 #x61)
  (:numpad2 #x62)
  (:numpad3 #x63)
  (:numpad4 #x64)
  (:numpad5 #x65)
  (:numpad6 #x66)
  (:numpad7 #x67)
  (:numpad8 #x68)
  (:numpad9 #x69)
  (:multiply #x6A)
  (:add #x6B)
  (:separator #x6C)
  (:substract #x6D)
  (:decimal #x6E)
  (:divide #x6F)
  (:f1 #x70)
  :f2
  :f3
  :f4
  :f5
  :f6
  :f7
  :f8
  :f9
  :f10
  :f11
  :f12
  :f13
  :f14
  :f15
  :f16
  :f17
  :f18
  :f19
  :f20
  :f21
  :f22
  :f23
  :f24
  (:numlock #x90)
  :scroll
  :oem-specific-0
  :oem-specific-1
  :oem-specific-2
  :oem-specific-3
  :oem-specific-4
  (:shift-l #xA0)
  :shift-r
  :control-l
  :control-r
  :menu-l
  :menu-r
  :browser-back
  :browser-forward
  :browser-refresh
  :browser-stop
  :browser-search
  :browser-favorites
  :browser-home
  :volume-mute
  :volume-down
  :volume-up
  :media-next-track
  :media-prev-track
  :media-stop
  :media-play-pause
  :launch-mail
  :maunch-media-select
  :launch-app1
  :launch-app2
  (:oem1 #xba) ;; varies, ";:" on us
  :oem-plus
  :oem-comma
  :oem-minus
  :oem-period
  :oem2
  (:oem3 #xC0)
  (:oem4 #xDB)
  :oem5
  :oem6
  :oem7
  :oem8
  (:oem102 #xE2)
  (:process-key #xE5)
  (:packet #xE7)
  (:attn #xF6)
  :crsel
  :exsel
  :ereof
  :play
  :zoom
  :no-name
  :pa1
  :oem-clear)

(defcenum system-command-type
  (:sc-minimize #xf020)
  (:sc-maximize #xf040)
  (:sc-restore #xf120))

(defcenum sw-cmd-show
  (:sw-hide 0)
  :sw-normal
  (:sw-show-normal 1)
  :sw-show-minimized
  :sw-maximize
  (:sw-show-maximized 3)
  :sw-show-no-activate
  :sw-show
  :sw-minimize
  :sw-show-min-no-activate
  :sw-show-na
  :sw-restore
  :sw-show-default
  :sw-force-minimize
  (:sw-max 11))

(defcenum remove-msg
  (:pm-no-remove 0)
  (:pm-remove 1))

(defcstruct rect
  (left :long)
  (top :long)
  (right :long)
  (bottom :long))

(defcenum display-settings-mode
  (:cds-update-registry 1)
  (:cds-test 2)
  (:cds-fullscreen 4)
  (:cds-global 8)
  (:cds-set-primary 16)
  (:cds-reset #x40000000)
  (:cds-setrect #x20000000)
  (:cds-no-reset #x10000000))

(defbitfield device-mode-fields
  (:dm-bits-per-pixel #x00040000)
  (:dm-pels-width #x00080000)
  (:dm-pels-height #x00100000)
  (:dm-display-frequency #x00400000))

(defbitfield swp-flags
  (:swp-no-size #x0001)
  (:swp-no-move #x0002)
  (:swp-no-zorder #x0004)
  (:swp-no-redraw #x0008)
  (:swp-no-activate #x0010)
  (:swp-frame-changed #x0020)
  (:swp-show-window #x0040)
  (:swp-hide-window #x0080)
  (:swp-no-copy-bits #x0100)
  (:swp-no-owner-zorder #x0200)
  (:swp-no-send-changing #x0400)
  (:swp-draw-frame #x0020)
  (:swp-no-reposition #x0200)
  (:swp-defer-erase #x2000)
  (:swp-async-window-pos #x4000))

(defcstruct devmode
  (device-name :char :count 32) ;; CCHDEVICENAME = 32 (winuser.h)
  (spec-version word)
  (driver-version word)
  (size word)
  (driver-extra word)
  (fields dword)
  (union-1 :short :count 8) ;; XXX: orientation data is here
  (color :short)
  (duplex :short)
  (y-resolution :short)
  (tt-option :short)
  (collate :short)
  (form-name :char :count 32) ;; CCHFORMNAME = 32
  (log-pixels word)
  (bits-per-pixel dword)
  (pels-width dword)
  (pels-height dword)
  (display-flags dword) ;; this is also dmNup
  (display-frequency dword)
  ;; WINVER >= 0x0400
  (icm-method dword)
  (icm-intent dword)
  (media-type dword)
  (dither-type dword)
  (reserved-1 dword)
  (reserved-2 dword)
  ;; WINVER >= 0x0500 || _WIN32_WINNT >= 0x0400
  (panning-width dword)
  (panning-height dword))

(defcenum (hit-test-code word)
  (:ht-error -2)
  (:ht-transparent -1)
  (:ht-nowhere 0)
  (:ht-client 1)
  (:ht-caption 2)
  (:ht-sys-menu 3)
  (:ht-size 4) (:ht-grow-box 4)
  (:ht-menu 5)
  (:ht-scroll 6)
  (:ht-v-scroll 7)
  (:ht-reduce 8) (:ht-min-button 8)
  (:ht-zoom 9) (:ht-max-button 9)
  (:ht-left 10)
  (:ht-right 11)
  (:ht-top 12)
  (:ht-top-left 13)
  (:ht-top-right 14)
  (:ht-bottom 15)
  (:ht-bottom-left 16)
  (:ht-bottom-right 17)
  (:ht-border 18)
  (:ht-close 20)
  (:ht-help 21)
)

(defcenum (system-metrics :int)
  (:arrange 56)
  (:clean-boot 67)
  (:c-monitors 80)
  (:c-mouse-buttons 43)
  (:convertible-slate-mode #x2003)
  (:cx-border 5)
  (:cx-cursor 13)
  (:cx-dialog-frame 7)
  (:cx-double-click 36)
  (:cx-drag 68)
  (:cx-edge 45)
  (:cx-fixed-frame 7)
  (:cx-focus-border 83)
  (:cx-frame 32)
  (:cx-full-screen 16)
  (:cx-h-scroll 21)
  (:cx-icon 11)
  (:cx-icon-spacing 38)
  (:cx-maximized 61)
  (:cx-max-track 59)
  (:cx-menu-check 71)
  (:cx-menu-size 54)
  (:cx-min 28)
  (:cx-minimized 57)
  (:cx-min-spacing 47)
  (:cx-min-track 34)
  (:cx-padded-border 92)
  (:cx-screen 0)
  (:cx-size 30)
  (:cx-size-frame 32)
  (:cx-small-icon 49)
  (:cx-small-caption-size 52)
  (:cx-virtual-screen 78)
  (:cx-v-scroll 2)
  (:cy-border 6)
  (:cy-caption 4)
  (:cy-cursor 14)
  (:cy-dialog-frame 8)
  (:cy-double-click 37)
  (:cy-drag 69)
  (:cy-edge 46)
  (:cy-fixed-frame 8)
  (:cy-focus-border 84)
  (:cy-frame 33)
  (:cy-full-screen 17)
  (:cy-h-scroll 3)
  (:cy-icon 12)
  (:cy-icon-spacing 39)
  (:cy-kanji-window 18)
  (:cy-maximized 62)
  (:cy-max-track 60)
  (:cy-menu 15)
  (:cy-menu-check 72)
  (:cy-menu-size 55)
  (:cy-min 29)
  (:cy-minimized 58)
  (:cy-min-spacing 48)
  (:cy-min-track 35)
  (:cy-screen 1)
  (:cy-size 31)
  (:cy-size-frame 33)
  (:cy-small-icon 50)
  (:cy-small-caption-size 51)
  (:cy-virtual-screen 79)
  (:cy-v-scroll 20)
  (:cy-b-thumb 9)
  (:dbcs-enabled 42)
  (:debug 22)
  (:digitizer 94)
  (:imm-enabled 82)
  (:maximum-touches 95)
  (:media-center 87)
  (:menu-drop-alignment 40)
  (:mideast-enabled 73)
  (:mouse-present 19)
  (:mouse-horizontal-wheel-present 91)
  (:mouse-wheel-present 75)
  (:network 63)
  (:pen-windows 41)
  (:remote-control #x2001)
  (:remote-session #x1000)
  (:same-display-format 81)
  (:secure 44)
  (:server-r2 89)
  (:show-sounds 70)
  (:shutting-down #x2000)
  (:slow-macchine 73)
  (:starter 88)
  (:swap-button 23)
  (:system-docked #x2004)
  (:tablet-pc 86)
  (:x-virtual-screen 76)
  (:y-virtual-screen 77))

(defcenum (device-caps :int)
  (:driver-version 0)
  (:technology 2)
  (:horizontal-size 4)
  (:vertical-size 6)
  (:horizontal-resolution 8)
  (:vertical-resolution 10)
  (:logical-pixels-x 88)
  (:logical-pixels-y 90)
  (:bits-per-pixel 12)
  (:planes 14)
  (:num-brushes 16)
  (:num-pens 18)
  (:num-fonts 22)
  (:num-colors 24)
  (:aspect-x 40)
  (:aspect-y 42)
  (:aspect-xy 44)
  (:clip-caps 26)
  (:size-palette 104)
  (:num-reserved 106)
  (:color-res 108)
  (:physical-width 110)
  (:physical-height 111)
  (:physical-offset-x 112)
  (:physical-offset-y 113)
  (:scaling-factor-x 114)
  (:scaling-factor-y 115)
  (:v-refresh 116)
  (:blt-alignment 119)
  (:shade-blend-caps 120)
  (:raster-caps 38)
  (:curve-caps 28)
  (:line-caps 30)
  (:polygonal-caps 32)
  (:text-caps 34)
  (:color-mgmt-caps 121)
 )
(defcenum (resource-names int-ptr)
  (:idc-arrow 32512) ;; Standard arrow
  (:idc-i-beam 32513) ;; I-beam
  (:idc-wait 32514) ;; Hourglass
  (:idc-cross 32515) ;; Crosshair
  (:idc-up-arrow 32516) ;; Vertical arrow
  
  (:idc-size 32640) ;; Obsolete for applications marked version 4.0 or later. Use IDC_SIZEALL.
  (:idc-icon 32641) ;; Obsolete for applications marked version 4.0 or later.
  (:idc-size-nwse 32642) ;; Double-pointed arrow pointing northwest and southeast
  (:idc-size-nesw 32643) ;; Double-pointed arrow pointing northeast and southwest
  (:idc-size-we 32644) ;; Double-pointed arrow pointing west and east
  (:idc-size-ns 32645) ;; Double-pointed arrow pointing north and south
  (:idc-size-all 32646) ;; Four-pointed arrow pointing north, south, east, and west
  (:idc-no 32648) ;; Slashed circle
  (:idc-hand 32649) ;; Hand
  (:idc-app-starting 32650) ;; Standard arrow and small hourglass
  (:idc-help 32651) ;; Arrow and question mark
  )

(defbitfield dcx-flags
  (:window #x1)
  (:cache #x2)
  (:no-reset-attrs #x4)
  (:clip-children #x8)
  (:clip-siblings #x10)
  (:parent-clip #x20)
  (:exclude-region #x40)
  (:intersect-region #x80)
  (:exclude-update #x100)
  (:intersect-update #x200)
  (:lock-window-update #x400)
  (:validate #x00200000)
  (:default-clip #x80000000)
)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun hwnd-cast (x)
    (if (= 8 (foreign-type-size :pointer))
        (make-pointer (ldb (byte 64 0) x))
        (make-pointer (ldb (byte 32 0) x)))))

(alexandria:define-constant +hwnd-no-topmost+ (hwnd-cast -2)
  :test 'pointer-eq)
(alexandria:define-constant +hwnd-topmost+ (hwnd-cast -1)
  :test 'pointer-eq)
(alexandria:define-constant +hwnd-top+ (hwnd-cast 0)
  :test 'pointer-eq)
(alexandria:define-constant +hwnd-bottom+ (hwnd-cast 1)
  :test 'pointer-eq)

(define-foreign-type resource-name ()
  ()
  (:actual-type :pointer)
  (:simple-parser resource-name))

(defmethod translate-to-foreign (value (type resource-name))
  (if (pointerp value)
      value
      (typecase value
        (string (values (foreign-string-alloc value) t))
        (symbol (make-pointer (foreign-enum-value 'resource-names value)))
        ((unsigned-byte 16) value))))

(defmethod free-translated-object (pointer (type resource-name) need-free)
  (when need-free
    (foreign-string-free pointer)))


(defmethod expand-to-foreign-dyn (value var body (type resource-name))
 (let ((constantp (constantp value))
       (original-value value))
  (alexandria:once-only (value)
    `(if (stringp ,value)
         (with-foreign-string (,var ,value)
           ,@body)
         (let ((,var ,(cond
                        ((and constantp
                              (foreign-enum-value 'resource-names original-value
                                                  :errorp nil))
                         (make-pointer
                          (foreign-enum-value 'resource-names original-value)))
                        ((and constantp
                              (typep original-value '(unsigned-byte 16)))
                         (make-pointer original-value))
                        (t
                         `(if (pointerp ,value)
                              ,value
                              (etypecase ,value
                                ((unsigned-byte 16)
                                 (make-pointer ,value))
                                (symbol
                                 (make-pointer
                                  (foreign-enum-value 'resource-names ,value)))))))))
           ,@body)))))




(define-foreign-library user32
  (t (:default "user32")))
(use-foreign-library user32)

(defmacro win-error (function)
  `(cerror "continue" "~a failed: ~a" ',function (get-last-error-string)))

(defmacro defefun (name ret &rest args)
  ;; wrapper for defcfun that checks return value and signals an error
  ;; when it returns false
  (flet ((err-check (r)
           (ecase ret
             ((bool :int) `(zerop ,r))
             ((handle hwnd hbitmap hdc) `(null-pointer-p ,r)))
           ))
   `(defun ,(second name) (,@ (mapcar 'first args))
      (let ((r (foreign-funcall ,(first name)
                                ,@ (loop for (a b) in args
                                         collect b
                                         collect a)
                                ,ret
                                )))
        (when ,(err-check 'r)
          ;; not sure if always erroring here is correct, at least one
          ;; function below uses "error" as "not found", so possibly
          ;; should make it configurable? and/or allow a custom error
          ;; handler?
          (cerror "continue"
                  "~a failed: ~a" ',(second name) (get-last-error-string)))
        r))))

(defcfun ("ShowCursor" show-cursor) :int
  (show bool))

(defefun ("GetCursorPos" %get-cursor-pos) bool
  (point (:pointer (:struct point))))

(defun get-cursor-pos ()
  (cffi:with-foreign-object (p '(:struct point))
    (%get-cursor-pos p)
    (values (cffi:foreign-slot-value p '(:struct point) 'x)
            (cffi:foreign-slot-value p '(:struct point) 'y))))

(defefun ("GetPhysicalCursorPos" %get-physical-cursor-pos) bool
  (point (:pointer (:struct point))))

(defun get-physical-cursor-pos ()
  (cffi:with-foreign-object (p '(:struct point))
    (%get-physical-cursor-pos p)
    (values (cffi:foreign-slot-value p '(:struct point) 'x)
            (cffi:foreign-slot-value p '(:struct point) 'y))))

(defefun ("SetCursorPos" set-cursor-pos) bool
  (x :int)
  (y :int))

(defefun ("SetPhysicalCursorPos" set-physical-cursor-pos) bool
  (x :int)
  (y :int))

(defcfun ("SetCursor" set-cursor) hcursor
  (hcursor hcursor))

(defcfun ("LoadCursorA" load-cursor) hcursor
  (hinstance hinstance)
  (cursor-name resource-name))

(defefun ("DestroyCursor" destroy-cursor) bool
  (hcursor hcursor))
#++
(glop2:with-application ()
  (let ((c (load-cursor (glop2/backend-win32::module-handle
                         (glop2::window-backend-connection
                          glop2::*application*))
                        :idc-wait)))
    (set-cursor-pos 10 10)
    (let ((old (set-cursor c)))
      (sleep 10)
      (set-cursor old))))

(defefun ("EnumDisplaySettingsA" enum-display-settings) bool
  (device-name :string) (mode-num dword) (dev-mode :pointer))

(defcfun ("ChangeDisplaySettingsA" change-display-settings) :long
  (dmode (:pointer (:struct devmode)))
  (flags dword))

(defcfun ("GetWindowLongA" get-window-long) :long
  (wnd hwnd) (index gwl-index))

(defcfun ("SetWindowLongA" set-window-long) :long
  (wnd hwnd) (index gwl-index) (new-long :unsigned-long))

(defefun ("SetWindowPos" set-window-pos) bool
  (wnd hwnd)
  (wnd-insert-after hwnd)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (flags swp-flags))

#++
(defun current-video-mode ()
  (with-foreign-object (dmode '(:struct devmode))
	(with-foreign-slots ((size bits-per-pixel pels-width pels-height display-frequency)
						 dmode (:struct devmode))
	  (setf size (foreign-type-size '(:struct devmode)))
	  (enum-display-settings (cffi:null-pointer) -1 dmode)
	  (make-video-mode :width pels-width
				 :height pels-height
				 :depth bits-per-pixel
				 :rate display-frequency))))
#++
(defun list-video-modes ()
  (with-foreign-object (dmode '(:struct devmode))
    (with-foreign-slots ((size bits-per-pixel pels-width pels-height display-frequency)
			 dmode (:struct devmode))
      (setf size (foreign-type-size '(:struct devmode)))
      (loop with mode-index = 0
	    for res = (enum-display-settings (cffi:null-pointer)
                                             mode-index dmode)
	    do (incf mode-index)
	    until (zerop res)
	    collect (make-video-mode :width pels-width
					   :height pels-height
					   :depth bits-per-pixel
					   :rate display-frequency)))))

#++
(defun set-video-mode (mode)
  (let ((width (glop::video-mode-width mode))
	(height (glop::video-mode-height mode))
	(depth (glop::video-mode-depth mode))
	(rate (glop::video-mode-rate mode)))
    (with-foreign-object (dmode '(:struct devmode))
      (with-foreign-slots ((size bits-per-pixel pels-width pels-height display-frequency fields)
			   dmode (:struct devmode))
	(setf size (foreign-type-size 'devmode))
	(enum-display-settings (cffi:null-pointer) -1 dmode)
	(setf pels-width width
	      pels-height height
	      display-frequency rate
	      bits-per-pixel depth
	      fields (foreign-bitfield-value 'device-mode-fields
                                             '(:dm-pels-width
					       :dm-pels-height
					       :dm-bits-per-pixel
					       :dm-display-frequency)))
	(change-display-settings dmode
				 (foreign-enum-value 'display-settings-mode
						     :cds-fullscreen))))))

#++
(defun default-video-mode ()
  (change-display-settings (cffi:null-pointer) 0))

(defun %set-borderless (wnd state &key keep-client-size)
  (multiple-value-bind (.x .y w h)
      (get-client-rect wnd)
    (multiple-value-bind (x y) (client-to-screen wnd .x .y)
      (let ((style (if state
                       '(:ws-popup :ws-clip-siblings :ws-clip-children)
                       '(:ws-overlapped-window
                         :ws-clip-siblings :ws-clip-children)))
            (ex-style (if state
                          '(:ws-ex-app-window :ws-ex-topmost)
                          '(:ws-ex-app-window :ws-ex-window-edge))))
        (set-window-long wnd :gwl-style
                         (foreign-bitfield-value 'wstyle style))
        (set-window-long wnd :gwl-ex-style
                         (foreign-bitfield-value 'wex-style ex-style)))
      ;; need to call set-window-pos for some changes to take effect
      (set-window-pos wnd (cffi:null-pointer) 0 0 0 0 '(:swp-no-move
                                                        :swp-no-size
                                                        :swp-frame-changed
                                                        :swp-no-zorder
                                                        :swp-no-copy-bits))
      ;; make sure client rect didn't change size
      (when keep-client-size
        (multiple-value-bind (.x2 .y2 w2 h2)
            (get-client-rect wnd)
          (multiple-value-bind (x2 y2) (client-to-screen wnd .x2 .y2)
            (unless (and (= x x2) (= y y2) (= w w2) (= h h2))
              (set-window-pos wnd (cffi:null-pointer) x y w h
                              '(:swp-no-zorder :swp-no-copy-bits)))))))))

(defun %maximize-window (wnd)
  (show-window wnd :sw-show-maximized))

(defun %restore-window (wnd)
  (show-window wnd :sw-show-normal))

(defun %set-fullscreen (wnd state)
  (if state
      (progn
        (%set-borderless wnd t)
        (%maximize-window wnd))
      (progn
        (%set-borderless wnd nil)
        (%restore-window wnd))))

(defefun ("GetClientRect" %get-client-rect) bool
  (wnd hwnd) (rect-out :pointer))

(defun get-client-rect (wnd)
  (with-foreign-object (rct '(:struct rect))
	(%get-client-rect wnd rct)
	(with-foreign-slots ((left top right bottom) rct (:struct rect))
	  (values left top
			  (- right left)
			  (- bottom top)))))

(defefun ("GetWindowRect" %get-window-rect) bool
  (wnd hwnd) (rect-out :pointer))

(defun get-window-rect (wnd)
  (with-foreign-object (rct '(:struct rect))
	(%get-window-rect wnd rct)
	(with-foreign-slots ((left top right bottom) rct (:struct rect))
	  (values left top
			  (- right left)
			  (- bottom top)))))

(defun get-client-area-offset (wnd)
  (multiple-value-bind (wx wy ww wh) (get-window-rect wnd)
    (declare (ignore ww wh))
    (multiple-value-bind (cx cy cw ch) (get-client-rect wnd)
      (declare (ignore cw ch))
      (values (- cx wx) (- cy wy)))))


(defefun ("AdjustWindowRectEx" %adjust-window-rect-ex) bool
  (rect (:pointer (:struct rect)))
  (style wstyle)
  (menu bool)
  (ex-style wex-style))

(defun adjust-window-rect-ex (x y width height &key style menu ex-style)
  (with-foreign-object (rect '(:struct rect))
    (with-foreign-slots ((left top right bottom) rect (:struct rect))
      (setf left x
            top y
            right (+ x width)
            bottom (+ y height))
      (%adjust-window-rect-ex rect style (if menu 1 0) ex-style)
      (values left top
              (- right left)
              (- bottom top)))))

(defefun ("ClientToScreen" %client-to-screen) bool
  (hwnd hwnd)
  (point (:pointer (:struct point))))

(defun client-to-screen (hwnd cx cy)
  (with-foreign-object (p '(:struct point))
    (with-foreign-slots ((x y) p (:struct point))
      (setf x cx y cy)
      (%client-to-screen hwnd p)
      (values x y))))

(defcfun ("GetUpdateRect" %get-update-rect) bool
  (wnd hwnd)
  (rect (:pointer (:struct rect)))
  (erase bool))

(defun get-update-rect (hwnd)
  (with-foreign-object (rect '(:struct rect))
    (with-foreign-slots ((left top right bottom) rect (:struct rect))
      (let ((ret (%get-update-rect hwnd rect 0)))
        (when (plusp ret) (values left top
                                  (- right left)
                                  (- bottom top)))))))

(defefun ("MoveWindow" move-window) bool
  (wnd hwnd) (x :int) (y :int) (width :int) (height :int)
  (repaint bool))

(defun set-geometry (wnd x y width height)
  ;; we specify position/size of client rect, convert to whole window
  (multiple-value-bind (ax ay aw ah)
      (adjust-window-rect-ex x y width height)
    (setf x ax y ay width aw height ah))
  (move-window wnd x y width height 1))

(defcfun ("SetCapture" set-capture) hwnd
  (wnd hwnd))

(defefun ("ReleaseCapture" release-capture) bool)

(defcfun ("GetDC" get-dc) hdc
  (wnd hwnd))

(defcfun ("GetDCEx" get-dc-ex) hdc
  (wnd hwnd)
  (clip hrgn)
  (flags dcx-flags))

(defcfun ("ReleaseDC" release-dc) :int
  (wnd hwnd) (dc hdc))

(defcfun ("PostQuitMessage" %post-quit-message) :void
  (exit-code :int))

(defcfun ("DefWindowProcA" %def-window-proc) :long
  (wnd hwnd) (msg :uint) (w-param wparam) (l-param lparam))

(defcfun ("GetMessageA" %get-message) bool
  (msg :pointer) (wnd hwnd) (filter-min :uint) (filter-max :uint))

(defcfun ("TranslateMessage" %translate-message) bool
  (msg :pointer))

(defcfun ("DispatchMessageA" %dispatch-message) bool
  (msg :pointer))

(defcfun ("PeekMessageA" %peek-message) bool
  (lpmsg :pointer) (h-wnd hwnd)
  (filter-min :uint) (filter-max :uint)
  (remove remove-msg))

(defefun ("GetKeyboardState" get-keyboard-state) bool
  (state-out :pointer))



(defcfun ("ToAscii" to-ascii) :int
  (vkey :uint)
  (scan-code :uint) (kbd-state :pointer) (buffer :pointer) (flags :uint))

(defcfun ("ToUnicode" to-unicode) :int
  (vkey :uint)
  (scan-code :uint) (kbd-state :pointer) (buffer :pointer) (buffer-size :int) (flags :uint))


;; XXX: we probably have problems with negative numbers here...
(defun low-word (value)
  (logand value #xFFFF))

(defun high-word (value)
  (logand (ash value -16) #xFFFF))

(defun win32-lookup-key (w-param l-param)
  (values (foreign-enum-keyword 'vkey-type w-param :errorp nil)
		  (with-foreign-object (kbd-state :char 256)
			(when (get-keyboard-state kbd-state)
			  (with-foreign-object (buffer :int32)
				(setf (mem-ref buffer :int32) 0)
				(let ((res (to-unicode (ldb (byte 32 0) w-param)
									   (ldb (byte 32 0) l-param)
									   kbd-state buffer 4 0)))
				  (case res
					(0 nil)
					(t (foreign-string-to-lisp buffer)))))))))



(defcfun ("RegisterClassA" %register-class) :int16
  (wndclass :pointer))

(defcfun ("RegisterClassExA" %register-class-ex) :int16
  (wndclass-ex :pointer))

(defefun ("GetClassInfoA" %get-class-info) bool
  (instance hinstance) (class-name :string) (wndclass :pointer))

(defefun ("UnregisterClassA" unregister-class) bool
  (class-name :string) (instance hinstance))

(defun class-exists-p (module-instance name)
  (with-foreign-object (class '(:struct wndclass))
    ;; %get-class-info errors when class doesn't exist, so just return
    ;; %nil then. possibly should check error manually and look for
    ;; %specific error?
    (ignore-errors
     (%get-class-info module-instance name class))))

(defun create-and-register-class (module-instance name window-proc)
  (with-foreign-object (class '(:struct wndclass))
    (with-foreign-slots ((style wndproc cls-extra wnd-extra instance icon cursor
                                br-background menu-name class-name)
                         class (:struct wndclass))
      (setf (foreign-slot-value class '(:struct wndclass)
                                'icon)
            (null-pointer))
      (setf style (foreign-bitfield-value 'class-style-flags
                                          '(:cs-hredraw :cs-vredraw :cs-own-dc))
            wndproc (get-callback window-proc)
            cls-extra 0
            wnd-extra 0
            instance module-instance
            icon	(null-pointer)
            cursor (null-pointer)
            br-background (null-pointer)
            menu-name (null-pointer)
            class-name name))
    (%register-class class)))

(defefun ("SetWindowTextA" set-window-text) bool
  (wnd hwnd) (title :string))

(defcfun ("CreateWindowExA" create-window-ex) hwnd
  (ex-style wex-style) (class-name :string) (win-name :string)
  (style wstyle) (x :int) (y :int) (width :int) (height :int)
  (parent hwnd) (menu hmenu) (instance hinstance) (param :pointer))

(defefun ("DestroyWindow" destroy-window) bool
  (wnd hwnd))

(defefun ("UpdateWindow" update-window) bool
  (wnd hwnd))

;; returns true if previously visible, false if previously hidden
(defcfun ("ShowWindow" show-window) bool
  (wnd hwnd) (cmd-show sw-cmd-show))

;; returns false if window wasn't brought to foreground, not sure if
;; that should error or not?
(defcfun ("SetForegroundWindow" set-foreground-window) bool
  (wnd hwnd))

(defefun ("BringWindowToTop" bring-window-to-top) bool
  (wnd hwnd))

(defcfun ("SetFocus" set-focus) hwnd
  (wnd hwnd))

(define-foreign-library kernel32
  (t (:default "kernel32")))
(use-foreign-library kernel32)

(defcfun ("GetModuleHandleW" get-module-handle) hmodule
  (module-name :string))

(defcfun ("GetLastError" get-last-error) :int32)

(cffi:defcfun (%format-message "FormatMessageW") dword
  (flags dword) ;; fixme: add bitfield def? (low byte is max width though)
  (source :pointer)
  (message-id dword)
  (language-id dword)
  (buffer :pointer)
  (size dword)
  (argumnents :pointer))

(defun get-last-error-string ()
  (let ((e (get-last-error))
        (bufsize (1- (expt 2 15)))) ;; max 64k bytes
    (with-foreign-object (buf :unsigned-short bufsize)
      (let ((c (%format-message #x1000 ;; format-message-from-system, no width
                                (null-pointer)
                                e
                                0 ;; default language
                                buf bufsize
                                (null-pointer))))
        (if (zerop c)
            (format nil "failed to get error string for error ~s? (~s)" e c)
            (string-trim '(#\space #\newline #\return #\tab)
                         (foreign-string-to-lisp buf :encoding :utf-16le)))))))


(defcfun ("GetVersion" %get-version) dword)

(defun get-version* ()
  (let ((v (%get-version)))
    (values (ldb (byte 8 0) v) ; major
            (ldb (byte 8 8) v) ; minor
            (ldb (byte 16 16) v) ; build
            )))

(defun get-version ()
  ;; Windows 10 Insider Preview         10.0*
  ;; Windows Server Technical Preview   10.0*
  ;; Windows 8.1                        6.3*
  ;; Windows Server 2012 R2             6.3*
  ;; * only reports 6.3+ with manifest, and then only reports specific
  ;;    version listed in manifest
  ;; Windows 8                          6.2
  ;; Windows Server 2012                6.2
  ;; Windows 7                          6.1
  ;; Windows Server 2008 R2             6.1
  ;; Windows Server 2008                6.0
  ;; Windows Vista                      6.0
  ;; Windows Server 2003 R2             5.2
  ;; Windows Server 2003                5.2
  ;; Windows XP 64-Bit Edition          5.2
  ;; Windows XP                         5.1
  ;; Windows 2000                       5.0
  (let ((v (%get-version)))
    (float (+ (ldb (byte 8 0) v)
              (/ (ldb (byte 8 8) v) 10)))))

(defmacro with-message ((msg) &body body)
  `(cffi:with-foreign-object (,msg '(:struct msg))
     ,@body))

(defbitfield monitor-info-flags
  (:primary 1))

(defcstruct monitor-info-ex
  (size dword)
  (monitor (:struct rect))
  (work (:struct rect))
  (flags monitor-info-flags)
  (device :char :count 32))

(defefun ("GetMonitorInfoA" %get-monitor-info) bool
  (hmonitor hmonitor)
  (lpmi (:pointer (:struct monitor-info-ex))))

(defun get-monitor-info (monitor)
  (with-foreign-object (lpmi '(:struct monitor-info-ex))
    (setf (foreign-slot-value lpmi '(:struct monitor-info-ex) 'size)
          (foreign-type-size '(:struct monitor-info-ex)))
    (when (%get-monitor-info monitor lpmi)
      (list*
       :device (foreign-string-to-lisp
                (foreign-slot-value lpmi '(:struct monitor-info-ex) 'device)
                :max-chars 32)
       (loop for (s k) in '((monitor :monitor)
                            (work :work-area)
                            (flags :flags))
             collect k
             collect (foreign-slot-value lpmi '(:struct monitor-info-ex) s))))))

(defcfun ("GetSystemMetrics" get-system-metrics) :int
  (index system-metrics))

(defcfun ("GetDeviceCaps" get-device-caps) :int
  (hdc hdc)
  (index device-caps))

(defcfun ("CreateDCA" %create-dc) hdc
  (driver :string)
  (device :string)
  (port :string)
  (pdm :pointer))

(defun create-dc (&key driver device port)
  (let ((dc (%create-dc (or driver (cffi:null-pointer))
                        (or device (cffi:null-pointer))
                        (or port (cffi:null-pointer))
                        ;; todo: devmode
                        (cffi:null-pointer))))
    (when (null-pointer-p dc)
      (win-error create-dc))
    dc))

(defefun ("DeleteDC" delete-dc) bool
  (hdc hdc))

(defefun ("EnumDisplayMonitors" %enum-display-monitors) bool
  (hdc hdc)
  (clip (:pointer (:struct rect)))
  (enum-proc :pointer)
  (data lparam))

(defvar %monitors%)
(cffi:defcallback enum-monitors bool ((hmonitor hmonitor)
                                      (hdc hdc)
                                      (clip-rect (:pointer (:struct rect)))
                                      (lparam lparam))
  (declare (ignorable lparam clip-rect hdc))
  (let ((mi (get-monitor-info hmonitor)))
   (push (append mi
                 (let ((dc (create-dc :device (getf mi :device))))
                   (unwind-protect
                        (loop for c in '(:horizontal-size :vertical-size
                                         :horizontal-resolution
                                         :vertical-resolution
                                         :logical-pixels-x :logical-pixels-y
                                         :bits-per-pixel
                                         :aspect-x :aspect-y :aspect-xy
                                         :v-refresh)
                              collect c
                              collect (get-device-caps dc c))
                     (delete-dc dc)))
                 )
         %monitors%))
  1)

(defun enum-display-monitors (&key hdc x1 y1 x2 y2
                                (callback (callback enum-monitors)))
  (let ((%monitors% nil))
    (if (or x1 y1 x2 y2)
        (with-foreign-object (clip '(:struct rect))
          (let ((vx (get-system-metrics :x-virtual-screen))
                (vcx (get-system-metrics :cx-virtual-screen))
                (vy (get-system-metrics :y-virtual-screen))
                (vcy (get-system-metrics :cy-virtual-screen)))
            (setf (foreign-slot-value clip '(:struct rect) 'left)
                  (or x1 vx))
            (setf (foreign-slot-value clip '(:struct rect) 'right)
                  (or x2 (+ vx vcx)))
            (setf (foreign-slot-value clip '(:struct rect) 'top)
                  (or y1 vy))
            (setf (foreign-slot-value clip '(:struct rect) 'bottom)
                  (or y2 (+ vy vcy))))
          (%enum-display-monitors (or hdc (cffi:null-pointer))
                                  clip
                                  callback
                                  0))
        (%enum-display-monitors (or hdc (cffi:null-pointer))
                                (cffi:null-pointer)
                                callback
                                0))
    %monitors%))
#++
(enum-display-monitors)
#++
((:DEVICE "\\\\.\\DISPLAY12" :MONITOR (BOTTOM 1080 RIGHT 3840 TOP 0 LEFT 1920)
  :WORK-AREA (BOTTOM 1080 RIGHT 3840 TOP 0 LEFT 1920) :FLAGS NIL
  :HORIZONTAL-SIZE 477 :VERTICAL-SIZE 268 :HORIZONTAL-RESOLUTION 1920
  :VERTICAL-RESOLUTION 1080 :LOGICAL-PIXELS-X 96 :LOGICAL-PIXELS-Y 96
  :BITS-PER-PIXEL 32 :ASPECT-X 36 :ASPECT-Y 36 :ASPECT-XY 51 :V-REFRESH 60)
 (:DEVICE "\\\\.\\DISPLAY11" :MONITOR (BOTTOM 1080 RIGHT 1920 TOP 0 LEFT 0)
  :WORK-AREA (BOTTOM 1080 RIGHT 1920 TOP 0 LEFT 0) :FLAGS (:PRIMARY)
  :HORIZONTAL-SIZE 598 :VERTICAL-SIZE 336 :HORIZONTAL-RESOLUTION 1920
  :VERTICAL-RESOLUTION 1080 :LOGICAL-PIXELS-X 96 :LOGICAL-PIXELS-Y 96
  :BITS-PER-PIXEL 32 :ASPECT-X 36 :ASPECT-Y 36 :ASPECT-XY 51 :V-REFRESH 60))
