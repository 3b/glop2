(defmethod make-blit-pointer ((w platform-window) width height)
  (error "don't know how to bit to this backend?")
  #++ (values blit pointer pointer-stride))

(defmethod %blit (w blit x y)
  (error "don't know how to blit to this backend?"))

(defmethod destroy-blit-pointer ((w platform-window) blit)
  (error "don't know how to bit to this backend?"))

(defmacro with-blit-pointer ((pointer win x y width height pointer-stride)
                             &body body)
  (alexandria:with-gensyms (blit)
   `(multiple-value-bind (,blit ,pointer ,pointer-stride)
        (make-blit-pointer ,win ,width ,height)
      (unwind-protect
           (progn
             (prog1 (progn ,@body)
               (%blit ,win ,blit ,x ,y)))
        (destroy-blit-pointer ,win ,blit)))))


(defmethod blit-buffer ((w window) buffer &key
                                            (xw 0) (yw 0)
                                            (xb 0) (yb)
                                            width height (stride width))
  "copy WIDTH by HEIGHT pixels from offset (XB,YB) in BUFFER to
offset (XW,YW) in W. If STRIDE is supplied, it specifies width of a
row in BUFFER in pixels. BUFFER can be a vector or 2d array,
containing either 32bit #xAARRGGBB values if element-type
is (unsigned-byte 32), or 4 octets per pixel in R,G,B,A order. BUFFER
can also be a 3d array containing individual R,G,B,A octets. If 2d or 3d,
dimension 0 is Y, dimension 1 is X. If 3d, dimension 2 is R,G,B,A."
  (let ((pixel-stride (etypecase buffer
                        ((or (array (unsigned-byte 32) 1)
                             (array * 2)
                             (array * 3))
                         1)
                        ((array * 1) 4))))
    (etypecase buffer
      (vector
       (unless (or stride width)
         (error "must supply width or stride when copying from vector"))
       (unless width
         (setf width stride))
       (unless stride
         (setf stride width))
       (assert (>= stride width))
       (unless height
         (setf height (floor (length buffer) (* pixel-stride stride)))))
      ((or (array * 2) (array * 3))
       (if width
           (assert (<= (+ xb width) (array-dimension buffer 1)))
           (setf width (- (array-dimension buffer 1) xb)))
       (if height
           (assert (<= (+ yb height) (array-dimension buffer 0)))
           (setf height (- (array-dimension buffer 0) yb)))))
    ;; not sure if these should error or not?
    (when (minusp xw)
      (setf width (max 0 (+ width xw)))
      (setf xw 0))
    (when (minusp yw)
      (setf height (max 0 (+ height yw)))
      (setf yw 0))
    (when (> (+ xw width) (width w))
      (setf width (max 0 (- (width w) xw))))
    (when (> (+ yw height) (height w))
      (setf height (max 0 (- (height w) yw))))

    (let ((row-stride (* pixel-stride stride)))
      (with-blit-pointer (p (platform-window w) xw yw width height pstride)
        (etypecase buffer
          ((vector (unsigned-byte 32))
           (loop for yp below height
                 for x1p = (* yp pstride)
                 for y1 from yb below (+ yb height)
                 for x1 = (+ (* yb row-stride) (* xb pixel-stride))
                 do (loop for x from x1 below (length buffer)
                          for xp from x1p below (+ x1p (* width 4)) by 4
                          for rgba = (aref buffer x)
                          ;; todo: see if we can write this as uint32?
                          do (setf (cffi:mem-aref p :uint8 (+ 0 xp))
                                   (ldb (byte 8 0) rgba))
                             (setf (cffi:mem-aref p :uint8 (+ 1 xp))
                                   (ldb (byte 8 8) rgba))
                             (setf (cffi:mem-aref p :uint8 (+ 2 xp))
                                   (ldb (byte 8 16) rgba))
                             (setf (cffi:mem-aref p :uint8 (+ 3 xp))
                                   (ldb (byte 8 32) rgba)))))
          ((vector (unsigned-byte 8))
           (loop for yp below height
                 for x1p = (* yp pstride)
                 for y1 from yb below (+ yb height)
                 for x1 = (+ (* yb row-stride) (* xb pixel-stride))
                 do (loop for x from x1 below (length buffer)
                          for xp from x1p below (+ x1p (* width 4))
                          do (setf (cffi:mem-aref p :uint8 0)
                                   (aref buffer x)))))
          ((vector float)
           (loop for yp below height
                 for x1p = (* yp pstride)
                 for y1 from yb below (+ yb height)
                 for x1 = (+ (* yb row-stride) (* xb pixel-stride))
                 do (loop for x from x1 below (length buffer)
                          for xp from x1p below (+ x1p (* width 4))
                          do (setf (cffi:mem-aref p :uint8 0)
                                   (min 255
                                        (max 0
                                             (floor
                                              (* 255
                                                 (aref buffer x)))))))))

          (vector
           (loop for yp below height
                 for x1p = (* yp pstride)
                 for y1 from yb below (+ yb height)
                 for x1 = (+ (* yb row-stride) (* xb pixel-stride))
                 do (loop for x from x1 below (length buffer)
                          for xp from x1p below (+ x1p (* width 4))
                          do (setf (cffi:mem-aref p :uint8 0)
                                   (min 255
                                        (max 0
                                             (aref buffer x)))))))


          ((array (unsigned-byte 32) 2)
           (loop for y from yb below (+ yb height)
                 for yp from 0
                 for x1p = (* yp pstride)
                 do (loop for x from xb below (+ xb width)
                          for xp from x1p by 4
                          for rgba = (aref buffer y x)
                          ;; todo: see if we can write this as uint32?
                          do (setf (cffi:mem-aref p :uint8 (+ 0 xp))
                                   (ldb (byte 8 0) rgba))
                             (setf (cffi:mem-aref p :uint8 (+ 1 xp))
                                   (ldb (byte 8 8) rgba))
                             (setf (cffi:mem-aref p :uint8 (+ 2 xp))
                                   (ldb (byte 8 16) rgba))
                             (setf (cffi:mem-aref p :uint8 (+ 3 xp))
                                   (ldb (byte 8 32) rgba)))))
          ((array (unsigned-byte 8) 2)
           (loop for y from yb below (+ yb height)
                 for yp from 0
                 for x1p = (* yp pstride)
                 do (loop for x from xb below (+ xb (* width 4))
                          for xp from x1p
                          do (setf (cffi:mem-aref p :uint8 xp)
                                   (aref buffer y x)))))
          ((array (unsigned-byte 8) 3)
           (loop with cc = (min 4 (array-dimension buffer 2))
                 for y from yb below (+ yb height)
                 for yp from 0
                 for x1p = (* yp pstride)
                 do (loop for x from xb below (+ xb width)
                          for xp from x1p by 4
                          do (loop for c below cc
                                   do (setf (cffi:mem-aref p :uint8 (+ xp x))
                                            (aref buffer y x c))))))
          ;; todo: float and generic 2/3d arrays?
          )))))


(defclass win32-blit ()
  ((dib :initarg :dib :reader dib)
   (pointer :initarg :pointer :reader pointer)))

(defmethod glop2::make-blit-pointer ((bc platform-window-win32) width height)
  (multiple-value-bind (dib pointer) (create-dib-section width height)
    (values (make-instance 'win32-blit :dib dib :pointer pointer)
            pointer)))

(defmethod glop2::destroy-blit-pointer (w (blit win32-blit))
  (delete-object (dib blit)))

(defmethod glop2::%blit ((w platform-window-win32) blit x y)
  (let* ((dc (dc w))
         (cdc (create-compatible-dc dc))
         ())
    ())
)
