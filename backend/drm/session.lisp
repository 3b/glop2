(in-package glop2/backend-drm)

;;; stuff to ask systemd for permission to access the hardware
;; todo: handle systems without systemd (might need to run as root, or
;; add user to some particular group?)

(defun take-device (path &key ((:bus *bus*) (application-bus)))
  (let ((rdev (nix:stat-rdev (nix:stat path))))
    (assert (not (zerop rdev)))
    (s-call "org.freedesktop.login1.Session" "TakeDevice"
             (ldb (byte 8 8) rdev)
             (ldb (byte 8 0) rdev))))

(defun release-device (fd &key ((:bus *bus*) (application-bus)))
  (let ((rdev (nix:stat-rdev (nix:fstat fd))))
    (unless (zerop rdev)
      (s-call "org.freedesktop.login1.Session" "ReleaseDevice"
               (ldb (byte 8 8) rdev)
               (ldb (byte 8 0) rdev)))))

(defun list-sessions (&key ((:bus *bus*) (application-bus)))
  (l1-call "org.freedesktop.login1.Manager" "ListSessions"))

(defun session-property (property &key ((:bus *bus*) (application-bus)))
  (dbus:get-property (bus *bus*)
                     "org.freedesktop.login1"
                     (default-session :bus *bus*)
                     "org.freedesktop.login1.Session"
                     property))

(defun seat-for-session (session &key ((:bus *bus*) (application-bus)))
  (dbus:get-property (bus *bus*)
                     "org.freedesktop.login1"
                     session
                     "org.freedesktop.login1.Session"
                     "Seat"))

(defun default-session (&key ((:bus *bus*) (application-bus)))
  ;; return session associated with this PID if it has a seat, or
  ;; first session with a seat from this user otherwise
  (format t "get session~%")
  (format t "pid = ~s~%" (nix:getpid))
  (finish-output)
  (let ((session (l1-call "org.freedesktop.login1.Manager"
                          "GetSessionByPID" (nix:getpid))))
    ;; if session doesn't have a seat (for example when running from
    ;; ssh), find a session for this user with a seat
    (format t "try ~s~%" session)
    (when (string= "" (car (seat-for-session session :bus *bus*)))
      (format t "  no seat~%")
      (setf session
            (loop with user = (nix:getuid)
                  for s in (list-sessions :bus *bus*)
                  for (seat-id uid uname seat session) = s
                  do (format t " ?? ~s~%" s)
                  when (and (= uid user) (string/= seat ""))
                    return (fifth s)))
      (format t "  session->~s~%" session))
    session))


(defun take-control (&key ((:bus *bus*) (application-bus)))
  (s-call "org.freedesktop.login1.Session" "TakeControl" nil))

(defun release-control (&key ((:bus *bus*) (application-bus)))
  (s-call "org.freedesktop.login1.Session" "ReleaseControl" nil))
