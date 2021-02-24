(in-package glop2/backend-drm)

;;; minimal epoll wrapper

(cffi:defcunion epoll-data
  (:pointer (:pointer :void))
  (:fd :int)
  (:u32 :uint32)
  (:u64 :uint64))

(cffi:defcenum epoll-op
  (:add 1) ;; Add a file descriptor to the interface.
  (:del 2) ;; Remove a file descriptor from the interface.
  (:mod 3));; Change file descriptor epoll_event structure.

(cffi:defbitfield (epoll-event-flags :uint32)
  (:in #x001)
  (:pri #x002)
  (:out #x004)
  (:rdnorm #x040)
  (:rdband #x080)
  (:wrnorm #x100)
  (:wrband #x200)
  (:msg #x400)
  (:err #x008)
  (:hup #x010)
  (:rdhup #x2000)
  (:exclusive #.(ash 1 28))
  (:wakeup #.(ash 1 29))
  (:oneshot #.(ash 1 30))
  (:et #.(ash 1 31)))

(cffi:defcstruct epoll-event
  (events epoll-event-flags)
  (data (:union epoll-data)))

(defun make-event-data (data &rest flags)
  (list 'data data 'events flags))

(cffi:defcfun ("epoll_create" epoll-create) :int
  ;; ignored but must be non-zero
  (size :int))

(cffi:defcenum epoll-create1-flags
  (:cloexec #8r02000000))

(cffi:defcfun ("epoll_create1" epoll-create1) :int
  (flags epoll-create1-flags))

(cffi:defcfun ("epoll_ctl" %epoll-ctl) :int
  (epfd :int)
  (op epoll-op)
  (fd :int)
  (event (:pointer (:struct epoll-event))))

(defun epoll-ctl (epfd op fd &rest flags-and-data)
  (format t "epollctl ~s ~s ~s~%~%" op fd flags-and-data)
  (let* ((data (when (eql (Car flags-and-data) :data)
                (cadr flags-and-data)))
        (flags (if data
                   (cddr flags-and-data)
                   flags-and-data)))
    (when (member :data flags)
      (error ":data must be before flags if supplied"))
    ;; `:data data` can be first 2 elements of flags-and-data, data
    ;; should be a list of (slot value) where slot is one of
    ;; :pointer,:fd,:u32,:u64 (value must be a foreign pointer if slot
    ;; is :pointer)
    (format t "  data = ~s, flags = ~s~%" data flags)
    (when (not flags) (break "no flags?"))
   (cffi:with-foreign-object (event '(:struct epoll-event))
     (setf (cffi:foreign-slot-value event '(:struct epoll-event) 'events)
           flags)
     (destructuring-bind (&optional (slot :u64) (value 0))
         data
       (setf (cffi:foreign-slot-value
              (cffi:foreign-slot-pointer event '(:struct epoll-event) 'data)
              '(:union epoll-data)
              slot)
             value))
     (format t "  ~s fd ~s, data ~s, flags ~s~%"
             op fd data flags)
     (%epoll-ctl epfd op fd event))))

(cffi:defcfun ("epoll_wait" %epoll-wait)
    ;; use osicat's "error on negative return" so we can use its
    ;; repeat-upon-eintr
    (nix::ERRNO-WRAPPER :INT :FUNCTION-NAME epoll-wait)
  (epfd :int)
  (events (:pointer (:struct epoll-event)))
  (max-events :int)
  (timeout :int))

#++
(cffi:defcfun ("epoll_pwait" epoll-pwait) :int
  (epfd :int)
  (events (:pointer (:struct epoll_event)))
  (max-events :int)
  (timeout :int)
  (sigmask (:pointer)))


(defun epoll-make-event-buffer (n)
  ;; to avoid consing, we can optionally pass a preallocated buffer to
  ;; wait, and fill it instead of consing a vector of lists to
  ;; return. For now, just storing alternating DATA and FLAGS
  (make-array (* n 2) :element-type '(unsigned-byte 64)
                      :initial-element 0))

(defun epoll-event-buffer-length (b)
  (floor (length b) 2))
(defun epoll-set-event-buffer-fd (b fd i)
  (setf (aref b (* i 2)) fd))
(defun epoll-set-event-buffer-flags (b flags i)
  (setf (aref b (1+ (* i 2))) flags))

(defun epoll-get-event-buffer-fd (b i)
  (aref b (* i 2)))
(defun epoll-get-event-buffer-flags (b i)
  (aref b (1+ (* i 2))))
(defun epoll-get-event-buffer-flag (b i flag &key return-extras)
  (let ((f (aref b (1+ (* i 2))))
        ;; fixme: generate this based on the cffi enum type?
        ;; inlined here to avoid hash table lookups etc
        (bit (ecase flag
               (:in #x001)
               (:err #x008)
               (:pri #x002)
               (:out #x004)
               (:rdnorm #x040)
               (:rdband #x080)
               (:wrnorm #x100)
               (:wrband #x200)
               (:msg #x400)
               (:hup #x010)
               (:rdhup #x2000)
               (:exclusive #.(ash 1 28))
               (:wakeup #.(ash 1 29))
               (:oneshot #.(ash 1 30))
               (:et #.(ash 1 31)))))
    (declare (type (unsigned-byte 32) f bit))
    (if return-extras
        (values (logtest f bit)
                (let ((f2 (ldb (byte 32 0) (logand f (lognot bit)))))
                  (when (plusp f2)
                    ;; not expected to have extra flags signalled, so this
                    ;; can be slow path
                    (cffi:foreign-bitfield-symbols 'epoll-event-flags
                                                   f))))
        (values (logtest f bit) nil))))

(defun epoll-wait (epfd &key max-events timeoutee
                          (loop-on-eintr t)
                          event-buffer)
  (when event-buffer
    (if max-events
        (assert (<= max-events (epoll-event-buffer-length event-buffer)))
        (setf max-events (epoll-event-buffer-length event-buffer))))
  (let ((max-events* (if (eql max-events nil)
                         1
                         max-events)))
    (cffi:with-foreign-object (events '(:struct epoll-event) max-events*)
      (let ((r (cond
                 ;; loop on eintr with a timeout
                 ((and loop-on-eintr timeout (not (minusp timeout)))
                  (format t "!!!wait loop ~s timeout ~s~%"
                          loop-on-eintr timeout)
                  (nix:repeat-upon-condition-decreasing-timeout
                      ((nix:eintr) rtimeout timeout)
                    (format t "wait...~%")
                    (%epoll-wait epfd events max-events* rtimeout)))
                 ;; loop on eintr with no (finite) timeout
                 (loop-on-eintr
                  #++(format t "!!!wait loop ~s no timeout ~s~%"
                          loop-on-eintr timeout)
                  (nix:repeat-upon-eintr
                    #++(format t "wait...~%")
                    (%epoll-wait epfd events max-events* (or timeout -1))))
                 (t
                  (format t "!!!wait no loop ~s ?timeout ~s~%"
                          loop-on-eintr timeout)
                  (%epoll-wait epfd events max-events* (or timeout -1))))))
        #++(format t "??? wait returned ~s / ~s~%" r max-events)
        (if (plusp r)
            (cond
              (event-buffer
               (loop
                 for i below r
                 for e = (cffi:mem-aptr events '(:struct epoll-event) i)
                 for flags = (cffi:mem-ref
                              (cffi:foreign-slot-pointer e
                                                         '(:struct epoll-event)
                                                         'events)
                              :uint32)
                 for fd = (cffi:foreign-slot-value
                           (cffi:foreign-slot-pointer e
                                                      '(:struct epoll-event)
                                                      'data)
                           '(:union epoll-data)
                           ':fd)
                 do (epoll-set-event-buffer-fd event-buffer fd i)
                    (epoll-set-event-buffer-flags event-buffer flags i))
               r)
              ;; if max-events was specified, return a vector of lists
              ;; describing the event ;; TODO: better API for return value
              (max-events
               (let ((a (make-array max-events)))
                 (loop
                   for i below r
                   do (setf (aref a i)
                            (cffi:mem-aref events '(:struct epoll-event) i)))
                 (values a r)))
              ;; if max-events wasn't supplied, just return the event
              ;; as a list
              (t
               (values (cffi:mem-ref events '(:struct epoll-event))
                       r)))
            ;; epoll return an error
            (values nil r))))))
