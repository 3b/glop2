(in-package #:glop2);;;; user API

;;; user should inherit their own window class from this
(defclass window ()
  ;; most of the contents of this class is in internal classes, see
  ;; 'redirect' macros below for available readers/writers/accessors
  ((platform-window :reader platform-window)
   (platform-context :reader platform-context)
   (event-mask :reader event-mask :initform (make-hash-table))
   (fb :Reader fb :initform nil))
  (:default-initargs :3d-api :gl))


;; for now, assuming window-backend and context-backend are
;; per-application, so set them there
(defmethod initialize-instance :after ((o window)
                                       &rest initargs
                                       &key (3d-api :gl))
  (unless (boundp '*application*)
    (warn "no *application* bound, creating global one")
    (setf *application*
          (make-application :3d-api 3d-api)))
  (setf (slot-value o 'platform-window)
        (apply #'make-instance (window-backend *application*)
               :window o initargs))
  (if (context-backend *application*)
      (setf (slot-value o 'platform-context)
            (apply #'make-instance (context-backend *application*) initargs))
      (warn "no context backend?"))
  (setf (slot-value (platform-window o) 'application) *application*))

(macrolet ((redirect-method (name from to)
             (let* ((name (alexandria:ensure-list name))
                    (r (gensym "R"))
                    (k (position '&key (cdr name))))
               (print
                (if k
                    (let* ((a (cdr name))
                           (req (subseq a 0 k))
                           (key (subseq a k)))
                      `(defmethod ,(car name)
                          ((object ,from)
                           ,@req
                           &rest ,r
                             ,@key)
                         (declare (ignore ,@(cdr key)))
                         (apply #',(car name) (,to object) ,@req ,r)))
                    `(defmethod ,(car name)
                         ((object ,from)
                          ,@ (cdr name))
                       (,(car name) (,to object)
                        ,@(cdr name)))))))
           (redirect-writer (name from to)
             (let* ((name (alexandria:ensure-list name))
                    (new (gensym "NEW"))
                    (r (gensym "R"))
                    (k (position '&key (cdr name))))
               (print
                (if k
                    (let* ((a (cdr name))
                           (req (subseq a 0 k))
                           (key (subseq a k)))
                      `(defmethod (setf ,(car name))
                           (,new (object ,from) ,@req &rest ,r ,@key)
                         (declare (ignore ,@(cdr key)))
                         (apply #'(setf ,(car name)) ,new (,to object) ,@req ,r)))
                    `(defmethod (setf ,(car name))
                         (,new (object ,from) ,@ (cdr name))
                       (,(car name) ,new (,to object)
                        ,@(cdr name))))))
)
           (redirect-accessor (name from to)
             `(progn
                (redirect-method ,name ,from ,to)
                (redirect-writer ,name ,from ,to)))
           (redirect-methods (from to &rest names)
             `(progn
                ,@ (loop for name in names
                         collect `(redirect-method ,name ,from ,to))))
           (redirect-writers (from to &rest names)
             `(progn
                ,@ (loop for name in names
                         collect `(redirect-writer ,name ,from ,to))))
           (redirect-accessors (from to &rest names)
             `(progn
                ,@ (loop for name in names
                         collect `(redirect-accessor ,name ,from ,to)))))
  (redirect-accessors
   window platform-window
   ;; accessors for window properties
   x y width height title
   ;; name of current cursor
   (cursor &key pointer)
)
  (redirect-methods window platform-window
   3d-api
   close-window
   (set-geometry x y width height)
   show-window
   hide-window
   (set-window-title title)
   raise-window
   lower-window
   swap-buffers
   (show-cursor &key pointer)
   (hide-cursor &key pointer)
   application
   ;; clip-cursor
   (create-cursor name cx cy bits)
   (get-cursor-position &key pointer)
   (get-cursor-position-using-backend &key pointer)
   (set-cursor-position x y &key pointer)
   (set-cursor-position-using-backend x y &key pointer)))

#++(fmakunbound '(setf cursor))
(defmethod get-cursor-position-using-backend ((window platform-window)
                                              &key pointer)
  (get-cursor-position-using-backend (glop2::window-backend-connection
                                      (application window))
                                     :pointer pointer))

(defmethod set-cursor-position-using-backend ((window platform-window) x y
                                              &key pointer)
  (set-cursor-position-using-backend (glop2::window-backend-connection
                                      (application window))
                                     x y :pointer pointer))

(defmethod get-cursor-position ((window platform-window) &key pointer)
  (format t "...")
  (get-cursor-position-using-backend (glop2::window-backend-connection
                                      (application window)) :pointer pointer))

(defmethod set-cursor-position ((window platform-window) x y &key pointer)
  (set-cursor-position-using-backend (glop2::window-backend-connection
                                      (application window))
                                     x y :pointer pointer))


(defmethod open-window ((w window) title width height
                        &key (x 0) (y 0) attribs
                          parent
                          (events '(on-key on-button on-mouse-wheel
                                    on-resize on-close))
                          background
                          )
  ;; not sure if this should keep any existing event mask?
  (clrhash (event-mask w))
  (loop for e in events do (setf (gethash e (event-mask w)) t))
  (open-window (platform-window w)
               title width height
               :x x :y y
               :parent parent
               :background background)
  (if (slot-boundp w 'platform-context)
      (open-context (platform-context w) (platform-window w) attribs)
      (Warn "no context")))


(defmethod destroy-fb ((win platform-window))
  (when (and (fb win)
             (fb-pointer (fb win)))
    (setf (slot-value (fb win) 'fb-pointer) nil)
    (static-vectors:free-static-vector
     (shiftf (slot-value (fb win) 'fb-vector) nil)))
  nil)


(defmethod close-window :before ((w window))
  (when (fb w)
    (destroy-fb (shiftf (slot-value w 'fb) nil))))

(defun %update-geometry (win x y width height)
  (when (or (/= x (x win))
            (/= y (y win))
            (/= width (width win))
            (/= height (height win)))
    (setf (slot-value win 'x) x
          (slot-value win 'y) y
          (slot-value win 'width) width
          (slot-value win 'height) height)
    t))

(defmacro with-blit ((win buffer stride
                      width height dx dy
                      &key (packed nil))
                     &body body)
  (alexandria:once-only (win width height dx dy)
    `(static-vectors:with-static-vector (,buffer
                                         (* ,width ,height ,(if packed 4 1))
                                         :element-type ,(if packed
                                                            ''(unsigned-byte 32)
                                                            ''(unsigned-byte 8)))
       (let ((,stride ,(if packed width `(* ,width 4))))
         (declare (ignorable ,stride))
         (prog1 (progn ,@body)
           #++(format *debug-io* "%blit ~sx~s to ~s,~s~%" ,width ,height ,dx ,dy)
           (%blit-pointer (platform-window ,win)
                          (static-vectors:static-vector-pointer ,buffer)
                          ,dx ,dy ,width ,height)))))
  )

(defmethod glop2::blit ((from window) fx fy w h
                        (to window) tx ty)
  (glop2::blit (platform-window from) fx fy w h (platform-window to) tx ty))


(defmethod glop2::blit ((from window) fx fy w h
                        to tx ty)
  (glop2::blit (platform-window from) fx fy w h to tx ty))

(defmethod glop2::blit (from fx fy w h
                        (to window) tx ty)
  (glop2::blit from fx fy w h (platform-window to) tx ty))


(defmethod create-pixmap ((a window) w h)
  (create-pixmap (platform-window a) w h))
