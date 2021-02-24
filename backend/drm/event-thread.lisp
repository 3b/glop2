(in-package glop2/backend-drm)


;;; various bits used to synchronize between main thread and event thread
(defclass event-thread-data ()
  (;; to implement vsync, we want to be able to block until a page
   ;; flip occurs. If not doing vsync, we need to drop flips if
   ;; previous hasn't occurred
   #++(flip-cond :initform (bt:make-condition-variable :name "page flip")
              :reader flip-cond)
   ;;mutex used for flip-cond
   #++(flip-lock :initform (bt:make-lock "page flip lock")
                 :reader flip-lock)
   ;; trying semaphore instead of condvar for vsync stuff
   (flip-semaphore :initform (bt:make-semaphore :name "page flip" :count 1)
                   :reader flip-semaphore)
   ;; used to transfer readable FDs and libinput events to main
   ;; thread. Usually shouldn't put too much in the channel unless
   ;; there are a bunch of inputs while main loop is in debugger or
   ;; something. FDs are limited to # of watched FDs
   (messages :initform (make-instance 'chanl:unbounded-channel)
             :reader messages)
   ;; set to 1 to pause sending of non-fd events (unsent events will
   ;; probably be dropped, but may track if we have sent the first of
   ;; a paired event and send 1 of the second, for example might send
   ;; a "button up" if there has been a previous "button down" sent)
   (pause-events :initform (vector nil) :reader %pause-events)
   ;; FD of a FIFO indicating event loop should exit (data on it is
   ;; probably ignored, we just exit when it is ready to read)
   ;; list of 2 values, read then write
   (exit-fifos :initform (multiple-value-list (nix:pipe)) :accessor exit-fifos)
   ;; epoll FD (we use epoll directly to get edge triggered and/or
   ;; one-shot polling)
   (epoll-fd :initform (epoll-create1 :cloexec) :accessor epoll-fd)
   ;; mapping of FD to object to be sent on MESSAGES when FD is ready
   (fd-mapping :initform (make-hash-table) :Reader fd-mapping)
   ;; code to run for specific FDs before (or instead of) sending message
   ;; return T to indicate event is handled, or NIL to send message
   (fd-thunks :initform (make-hash-table) :reader fd-thunks)
   ;; thread object for event thread
   (event-thread :initform nil :reader event-thread)))

(defmethod pause-events ((etd event-thread-data))
  (svref (%pause-events etd) 0))

(defmethod (setf pause-events) (new (etd event-thread-data))
  (let* ((v (%pause-events etd)))
    (declare (type simple-vector v))
    (loop for old = (svref v 0)
          until (or (eql old new)
                    (atomics:cas (svref v 0) old new)))
    new))

#++
(defmethod epoll-fd ((f integer)) f)


(defun dbg (fmt &rest args)
  (declare (ignorable fmt args))
  #++(let ((s (apply #'format nil fmt args)))
    (format sb-sys:*stderr* "~a" s)
    (format t "~a" s)))

(defun event-loop (etd)
  #++
  (let ((l (multiple-value-list (epoll-wait (epoll-fd etd) :loop-on-eintr t)))
        (e (nix:strerror (nix:get-errno))))
    (format t "got ev ~s ~s~%" l e)
    (format sb-sys:*stderr* "got ev ~s ~s~%" l e))
  (let ((event-buffer (epoll-make-event-buffer 10)))
    (loop named waiter
          for c = (epoll-wait (epoll-fd etd) :loop-on-eintr t
                                             :event-buffer event-buffer)
          unless c do (dbg "epoll-wait returned nil?~%")
          while c
          do (loop for i below c
                   for fd = (epoll-get-event-buffer-fd event-buffer i)
                   for tag = (gethash fd (fd-mapping etd))
                   for thunk = (gethash fd (fd-thunks etd))
                   for handled = nil
                   do (multiple-value-bind (in extras)
                          (epoll-get-event-buffer-flag
                           event-buffer i :in :return-extras t)
                        #++(dbg "got event for ~s = ~s: in=~s, x = ~s~%"
                             fd tag in extras)
                        (when (and in thunk)
                          ;; some tags need special handling
                          (setf handled (funcall thunk)))
                        (unless handled
                          (cond
                            (extras
                             (chanl:send (messages etd)
                                         ;; :IN is included in extras if set
                                         (list* tag extras)))
                            (in
                             (chanl:send (messages etd) tag))
                            ;; shouldn't get this
                            (t (break "??")
                               (chanl:send (messages etd)
                                           (list tag :???))))))
                      (when (eql tag '%exit)
                        (return-from waiter nil)))))
  (format sb-sys:*stderr* "exiting event loop~%"))


;; epoll man pages say it is safe to add/remove from other threads
;; while waiting
(defun add-fd-to-event-loop (etd fd &key (tag fd) thunk)
  (setf (gethash fd (fd-mapping etd)) tag)
  (setf (gethash fd (fd-thunks etd)) thunk)
  (epoll-ctl (epoll-fd etd) :add fd
                            :data (list :fd fd)
                            :IN ;; readable
                            :pri ;; exceptional situations
                            :err ;; error condition, or reader closed
                            :rdhup ;; writer closed
                            :hup ;; hang-up on FD
                            :et)) ;; edge triggered

(defun remove-fd-from-event-loop (etd fd)
  (epoll-ctl (epoll-fd etd) :del fd (make-event-data 0))
  (remhash fd (fd-mapping etd))
  (remhash fd (fd-thunks etd)))

(defun start-event-thread ()
  (with-simple-restart (continue "continue")
    (let ((etd (make-instance 'event-thread-data)))
      ;; most initialization is handled by initforms, but we need to add
      ;; the read FD to the epoll FD
      (format t "add fd ~s to event loop~%" (first (exit-fifos etd)))
      (add-fd-to-event-loop etd (first (exit-fifos etd)) :tag '%exit)
      (setf (slot-value etd 'event-thread)
            (bordeaux-threads:make-thread (lambda ()
                                            (event-loop etd))
                                          :name "glop2 event thread"))
      (format t "started event thread ~s~%" (event-thread etd))
      etd)))

(defun shutdown-event-thread (etf)
  (format t "shutdown-event-thread ~s~%" etf)
  (when etf
    (with-simple-restart (continue "continue")
      ;; write to FIFO telling it to shut down
      (cffi:with-foreign-string ((p l) "exit") ;; contents ignored for now
        (nix:write (second (exit-fifos etf)) p l))
      ;; wait for it to exit
      (format t "join thread ...~%")
      (bt:with-timeout (1000)
        (bt:join-thread (event-thread etf)))
      (format t "close fifos...~%")
      ;; close FD and FIFO
      (map 'nil 'nix:close (shiftf (exit-fifos etf) nil))
      (nix:close (shiftf (epoll-fd etf) nil))))
  (format t "done~%"))



