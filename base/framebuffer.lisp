(in-package #:glop2);;;; user API

 ;;;; wrapper for a 'framebuffer' for offscreeen drawing
 ;;(defclass framebuffer ()
 ;;  ((width :reader width :initform 0 :initarg :width)
 ;;   (height :reader height :initform 0 :initarg :height)
 ;;   (stride :reader stride :initform 0 :initarg :stride)
 ;;   ;; :1-1, :stretch, :smooth-stretch, :integer-stretch
 ;;   (blt-mode :accessor blt-mode :initform :1-1 :initarg :blt)
 ;;   ;; :center, :ne, :n, :nw, :w, :sw, :s, :se
 ;;   (position-mode :accessor position-mode :initform :center :initarg :position)
 ;;   ;; for double-buffering
 ;;   (buffer-index :accessor buffer-index :initform 0)
 ;;   ;; array of lists of dirty rectangles for given buffer
 ;;   (dirty-rectangles :reader)
 ;;   ;; held by reader when blitting to screen, by writer when resizing
 ;;   ;; or swapping buffers (not locked while writing to buffer, since
 ;;   ;; it would block UI thread, use double buffering to avoid partial
 ;;   ;; updates)
 ;;   (lock :reader lock :initform (bt:make-lock))))
 ;;
 ;;(* 1920 1080 4 4)33 177 600
 ;;
 ;;(defclass vector-framebuffer (framebuffer)
 ;;  ((vectors :reader vectors)))
 ;;
 ;;(defclass pointer-framebuffer (framebuffer)
 ;;  ((pointers :reader pointers)))
 ;;
 ;;(defmethod draw-buffer)
 ;;(defclass )
 ;;
 ;;
 ;;
 ;;(defmethod make-fb-vector ((w null))
 ;;  nil)
 ;;
 ;;(defmethod make-fb-pointer ((w null))
 ;;  nil)
 ;;
 ;;(defmethod make-fb-vector ((win ))
 ;;  (or (make-fb-vector (platform-context win))
 ;;      (make-fb-vector (platform-window win))))
 ;;
 ;;(defmethod make-fb-pointer ((win window))
 ;;  (or (make-fb-pointer (platform-context win))
 ;;      (make-fb-pointer (platform-window win))))
 ;;
 ;;
 ;;(defmethod fb-size ((w platform-window))
 ;;  (* (width w) (height w) 4))
 ;;(defmethod fb-stride ((w platform-window))
 ;;  (* (width w) 4))
 ;;
 ;;(defmethod maybe-resize-framebuffer ((win window))
 ;;  ;; todo: possibly should try to reuse old contents on resize?
 ;;  (when (fb win)
 ;;    (let* ((pw (platform-window win))
 ;;           (s (fb-size pw))
 ;;           (pointer (and (fb win) (fb-pointer (fb win)))))
 ;;      (when (/= s (size (fb win)))
 ;;        (destroy-fb win)
 ;;        (setf (slot-value win 'fb)
 ;;              (if pointer
 ;;                  (make-fb-pointer win)
 ;;                  (make-fb-vector win)))))))
 ;;
 ;;
 ;;(defmethod make-fb-vector ((win platform-window))
 ;;  (let ((size (fb-size win)))
 ;;    (make-instance 'framebuffer
 ;;                   :width (width win)
 ;;                   :height (height win)
 ;;                   :stride (fb-stride win)
 ;;                   :size size
 ;;                   :vector (make-array size
 ;;                                       :element-type '(unsigned-byte 8)
 ;;                                       :initial-element #xff))))
 ;;
 ;;(defmethod make-fb-pointer ((win platform-window))
 ;;  (let* ((size (fb-size win))
 ;;         (f (static-vectors:make-static-vector size
 ;;                                               :initial-element #xff)))
 ;;    (make-instance 'framebuffer
 ;;                   :width (width win)
 ;;                   :height (height win)
 ;;                   :stride (fb-stride win)
 ;;                   :size size
 ;;                   :vector f
 ;;                   :pointer (static-vectors:static-vector-pointer f))))
 ;;
 ;;(defmethod fb-width ((w window))
 ;;  (when (fb w) (width (fb w))))
 ;;(defmethod fb-height ((w window))
 ;;  (when (fb w) (height (fb w))))
 ;;(defmethod fb-stride ((w window))
 ;;  (when (fb w) (stride (fb w))))
 ;;
 ;;(defmethod fb-vector ((win window))
 ;;  (cond
 ;;    ((fb win)
 ;;     ;; possibly should allow using the fb-vector of a static-vectors
 ;;     ;; backed pointer, but that would encourage unportable behavior,
 ;;     ;; so not aloowed for now.
 ;;     (if (fb-pointer (fb win))
 ;;         (error "already using foreign-pointer framebuffer for window ~s" win)
 ;;         (progn
 ;;           (maybe-resize-framebuffer win)
 ;;           (fb-vector (fb win)))))
 ;;    (t
 ;;     (fb-vector
 ;;      (setf (slot-value win 'fb) (make-fb-vector win))))))
 ;;
 ;;(defmethod fb-pointer ((win window))
 ;;  (cond
 ;;    ((fb win)
 ;;     (if (fb-vector (fb win))
 ;;         (error "already using cl-vector framebuffer for window ~s" win)
 ;;         (progn
 ;;           (maybe-resize-framebuffer win)
 ;;           (fb-pointer (fb win)))))
 ;;    (t
 ;;     (fb-vector
 ;;      (setf (slot-value win 'fb) (make-fb-pointer win))))))
