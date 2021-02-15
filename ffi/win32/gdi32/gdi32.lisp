(in-package glop2/ffi-win32-gdi32)

(defctype hbitmap handle)

(defcstruct pixel-format-descriptor
  (size :int16)
  (version :int16)
  (flags :int32)
  (pixel-type :int8)
  (color-bits :int8)
  (red-bits :int8)
  (red-shift :int8)
  (green-bits :int8)
  (green-shift :int8)
  (blue-bits :int8)
  (blue-shift :int8)
  (alpha-bits :int8)
  (alpha-shift :int8)
  (accum-bits :int8)
  (accum-red-bits :int8)
  (accum-green-bits :int8)
  (accum-blue-bits :int8)
  (accum-alpha-bits :int8)
  (depth-bits :int8)
  (stencil-bits :int8)
  (aux-buffers :int8)
  (layer-type :int8)
  (reserved :int8)
  (layer-mask :int32)
  (visible-mask :int32)
  (damage-mask :int32))

(defbitfield (pfd-flags :int32)
  (:pfd-draw-to-window 4)
  (:pfd-draw-to-bitmap 8)
  (:pfd-support-gdi 16)
  (:pfd-support-opengl 32)
  (:pfd-generic-accelerated #x00001000)
  (:pfd-generic-format 64)
  (:pfd-need-palette 128)
  (:pfd-need-system-palette #x00000100)
  (:pfd-double-buffer 1)
  (:pfd-stereo 2)
  (:pfd-swap-layer-buffers  #x00000800)
  (:pfd-depth-dont-care     #x20000000)
  (:pfd-double-buffer-dont-care #x40000000)
  (:pfd-stereo-dont-care #x80000000)
  (:pfd-swap-copy #x00000400)
  (:pfd-swap-exchange #x00000200)
  (:pfd-support-composition #x00008000))

(defcenum pfd-pixel-type
  (:pfd-type-rgba 0)
  (:pfd-type-color-index 1))

(defcenum (dib-usage :int)
  (:pal-colors 1)
  (:rgb-colors 0)
  (:rgb 0))

(defcenum (bitmap-info-compression dword)
  (:rgb 0)
  (:rle8 1)
  (:rle4 2)
  (:bitfields 3)
  ;; (:jpeg ?)
  ;; (:png ?)
  )

(defcstruct bitmap-info-header
  (size dword)
  (width long)
  (height long)
  (planes word)
  (bit-count word)
  (compression bitmap-info-compression)
  (size-image dword)
  (x-pixels-per-meter long)
  (y-pixels-per-meter long)
  (color-used dword)
  (color-important dword))
;; type bitmap-info is bitmap-info-header + 32bit RGBA data or 16bit
;; pallete indices, so not defining specially
(defcstruct bitmap-info
  (header (:struct bitmap-info-header))
  (colors :uint32 :count 0))

(defcenum (blt-rops dword)
  (:src-copy #x00cc0020)
  (:src-paint #x00ee0086)
  (:src-and #x008800c6)
  (:src-invert #x00660046)
  (:src-erase #x00440328)
  (:not-src-copy #x00330008)
  (:not-src-erase #x001100a6)
  (:merge-copy #x00c000ca)
  (:merge-paint #x00bb0226)
  (:pat-copy #x00f00021)
  (:pat-paint #x00fb0a09)
  (:pat-invert #x005a0049)
  (:dst-invert #x00550009)
  (:blackness #x00000042)
  (:whiteness #x00ff0062)
  (:no-mirror-bitmap #x80000000)
  (:capture-blt #x40000000))

(define-foreign-library gdi32
  (t (:default "gdi32")))
(use-foreign-library gdi32)

(defcfun ("ChoosePixelFormat" %choose-pixel-format) :int
  (dc hdc) (pfd :pointer))

(defcfun ("SetPixelFormat" %set-pixel-format) bool
  (dc hdc) (pixel-format :int) (pfd :pointer))

(defun choose-pixel-format (dc &key (rgba t)
                                 (double-buffer t)
                                 stereo
                                 (red-size 0)
                                 (green-size 0)
                                 (blue-size 0)
                                 (alpha-size 0)
                                 (depth-size 0)
                                 accum-buffer
                                 (accum-red-size 0)
                                 (accum-green-size 0)
                                 (accum-blue-size 0)
                                 stencil-buffer (stencil-size 0))
  (declare (ignore stencil-buffer)
           (ignorable stereo))
  (with-foreign-object (pfd '(:struct pixel-format-descriptor))
    (with-foreign-slots ((size version flags pixel-type color-bits
                               red-bits green-bits blue-bits alpha-bits
                               accum-bits accum-red-bits accum-green-bits accum-blue-bits
                               stencil-bits
                               depth-bits)
                         pfd
                         (:struct pixel-format-descriptor))
      (setf size (foreign-type-size '(:struct pixel-format-descriptor))
            version 1
            flags (foreign-bitfield-value 'pfd-flags
                                          (list :pfd-draw-to-window :pfd-support-opengl
                                                :pfd-support-composition
                                                (if double-buffer
                                                    :pfd-double-buffer
                                                    :pfd-double-buffer-dont-care)
                                                ;; FIXME: there's a problem with :pfd-stereo-dont-care
                                                ;; (if stereo
                                                ;;     :pfd-stereo
                                                ;;     :pfd-stereo-dont-care)
                                                ))
            pixel-type (foreign-enum-value 'pfd-pixel-type
                                           (if rgba :pfd-type-rgba :pfd-type-color-index))
            color-bits 32 ;; we want proper RGBA but not sure to understand this struct field
            red-bits red-size
            green-bits green-size
            blue-bits blue-size
            alpha-bits alpha-size
            accum-bits (if accum-buffer
                           (+ accum-red-size accum-green-size accum-blue-size)
                           0)
            accum-red-bits accum-red-size
            accum-green-bits accum-green-size
            accum-blue-bits accum-blue-size
            depth-bits depth-size
            stencil-bits stencil-size))
    (let ((fmt (%choose-pixel-format dc pfd)))
      (%set-pixel-format dc fmt pfd)
      fmt)))

(defcfun ("SwapBuffers" swap-buffers) bool
  (dc hdc))

(glop2/ffi-win32::defefun ("CreateCompatibleBitmap" create-compatible-bitmap)
  hbitmap
  (hdc hdc)
  (cx :int)
  (cy :int))

(glop2/ffi-win32::defefun ("CreateCompatibleDC" create-compatible-dc) hdc
  (hdc hdc))

(glop2/ffi-win32::defefun ("CreateDIBSection" %create-dib-section) hbitmap
  (hdc hdc)
  (bmi (:pointer (:struct bitmap-info)))
  (usage dib-usage)
  (bits (:pointer :pointer))
  (hsection handle)
  (offset dword))

(defmacro with-bitmap-info ((var w h) &body body)
  `(with-foreign-object (,var '(:struct bitmap-info))
     (with-foreign-slots ((size width height planes bit-count compression
                                size-image color-used)
                          ,var
                          (:struct bitmap-info-header))
       (setf size (foreign-type-size '(:struct bitmap-info))
             width ,w
             ;; negative y = top-down
             height ,h
             planes 1
             bit-count 32
             compression :rgb
             size-image (abs (* ,w ,h 4))
             color-used 0)
       ,@body)))

(defun create-dib-section (w h)
  (with-foreign-object (bits :pointer)
    (with-bitmap-info (bmi w h)
      (let ((r (%create-dib-section (null-pointer) bmi
                                    :rgb-colors bits (null-pointer) 0)))
        (values r (mem-ref bits :pointer))))))

(glop2/ffi-win32::defefun ("DeleteObject" delete-object) bool
  (h handle))

(glop2/ffi-win32::defefun ("SelectObject" select-object) handle
  (hdc hdc)
  (obj handle))

(defmacro with-selected-object ((dc object) &body body)
  (alexandria:with-gensyms (old)
    `(let ((,old (select-object ,dc ,object)))
       (progn ,@body)
       (select-object ,dc ,old))))

(glop2/ffi-win32::defefun ("GdiFlush" gdi-flush) bool
  )


(glop2/ffi-win32::defefun ("SetDIBits" %set-di-bits) :int
  (hdc hdc)
  (ddb hbitmap)
  (start :uint)
  (lines :uint)
  (bits :pointer)
  (info (:pointer (:struct bitmap-info)))
  (color-use dib-usage))


(glop2/ffi-win32::defefun ("BitBlt" bit-blt) bool
  (dest-dc hdc)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (src-dc hdc)
  (x1 :int)
  (x2 :int)
  (rop blt-rops))



(Defun set-di-bits (dc ddb pointer w h &key (start 0) lines)
  (when (or (zerop w) (zerop h)
            (zerop (or lines (- h start))))
    (error "~s ~s ~s ~s?" w h start lines))
  (with-bitmap-info (bmi w (- h))
    (%set-di-bits dc ddb start (or lines (- h start)) pointer bmi :rgb)))
