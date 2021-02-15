(in-package #:glop2)


;;; Events handling
;; events are internally stored as a list like (on-foo window arg1
;; arg2 ...)  suitable to (apply #'funcall evt), see the defgenerics
;; later in file for arguments. Eventually, this will be implemented


;;;
(defun push-event (window evt &rest args)
  "Push an artificial event into the event processing system.
Note that this has no effect on the underlying window system."
  (push (list* evt window args)
        (pushed-events (application window))))



;; eventually event handling might be mostly non-consing, which will
;; require reusing storage, so %next-event is the non-consing
;; api, and next-event makes a copy if needed
(defmethod %next-event ((application application) &key blocking window)
  (when window
    ;; todo:implement per-window event queues, and only return next
    ;; event for that window
    )
  (let ((*application* application))
    #++(when (pushed-events application)
         (break "app"))
    (let ((e (or (pop (pushed-events application))
                 (%next-event (window-backend-connection application)
                              :blocking blocking))))
      e)))

(defmethod %next-event ((win window) &key blocking)
  (%next-event (application win) :blocking blocking :window win))

(defmethod next-event ((win window) &key blocking)
  (copy-seq (%next-event win :blocking blocking)))

(defmethod next-event ((app application) &key blocking)
  (copy-seq (%next-event app :blocking blocking)))



(defun push-close-event (window)
  "Push an artificial :close event into the event processing system."
  (push-event window 'on-close))

(defgeneric next-event (window &key blocking)
  (:documentation
   "Returns next available event for manual processing.
   If :blocking is true, wait for an event."))

;; method based event handling
(defun dispatch-events (&key (application *application*) blocking)
  "Process all pending system events and call corresponding methods.
When :blocking is non-nil calls event handling func that will block
until an event occurs.
Returns NIL on ON-DESTROY event of last open window, T otherwise."
  (loop for evt = (%next-event application :blocking blocking)
        for (ev w) = evt
        while evt
        when (eql ev :quit)
          do (format t "got quit message")
          and return nil
        else do (with-simple-restart (continue "Ignore ~a event" ev)
                  (when (gethash ev (event-mask w))
                    (apply #'funcall evt)))
        finally (return t)))


;; implement these to handle events
(defgeneric on-key (window pressed keycode keysym string))
(defgeneric on-button (window pressed button))
(defgeneric on-mouse-motion (window x y dx dy))
(defgeneric on-mouse-wheel (window wheel delta)
  ;; delta is float, +-1.0 = 1 click on wheel, but might be more
  ;; frequent but smnaller values on smooth spinning wheels, so
  ;; accumulate values if not doing fine-scrolling.
  )
(defgeneric on-close (window))
(defgeneric on-start-resize (window))
(defgeneric on-resize (window x y w h))
(defgeneric on-end-resize (window))

;; these are here for completeness but default methods are provided
(defgeneric on-visibility (window visible))
(defgeneric on-focus (window focused))

(defmethod on-visibility (window visible)
  (declare (ignore window visible)))
(defmethod on-focus (window focused-p)
  (declare (ignore window focused-p)))

(defmethod on-paint (window x1 y1 w h)
  (declare (ignore window x1 y1 w h)))


#++
(defmacro with-idle-forms (window &body idle-forms)
  (let ((blocking (unless idle-forms t))
        (res (gensym)))
    `(loop with ,res = (dispatch-events ,window :blocking ,blocking)
           while ,res
           do ,(if idle-forms
                   `(progn ,@idle-forms)
                   t))))

#++
(defmacro with-window ((win-sym title width height &rest attribs) &body body)
  "Creates a window and binds it to WIN-SYM.  The window is detroyed when body exits."
  `(let ((,win-sym (create-window ,title ,width ,height
                                  ,@attribs)))
     (when ,win-sym
       (unwind-protect (progn ,@body)
         (destroy-window ,win-sym)))))

;; multiple windows management
#++
(defun set-gl-window (window)
  "Make WINDOW current for GL rendering."
  (attach-gl-context window (window-gl-context window)))

;;; Keyboard stuff
(defvar *ignore-auto-repeat* nil
  "When set to NIL, holding a key press will generate a sequence of key-press events.
Otherwise, only one key-press event will be triggered.")

(defvar %key-states% (make-array #xffff :initial-element nil))

(defun key-pressed (keycode)
  (aref %key-states% keycode))

(defsetf key-pressed (keycode) (value)
  `(setf (aref %key-states% ,keycode) ,value))
