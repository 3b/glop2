(defpackage :glop2-test
  (:use :cl :glop2))
#++
(require 'glop2)
(in-package glop2-test)
(defclass test1 (window)
  ())

(defmethod on-start-resize ((w test1))
  (format t "start resize ~s~%" w))
(defmethod on-resize ((w test1) x y wx wy)
  (format t "  resize ~s: ~sx~s+~s+~s~%" w wx wy x y))
(defmethod on-end-resize ((w test1))
  (format t "end resize ~s~%" w))

(defmethod on-close ((w test1))
  (format t "close ~s~%" w)
  (close-window w))

(defmethod on-draw ((w test1))
  (format t "draw ~s~%" w))

(defmethod on-mouse-motion ((w test1) x y dx dy)
  #++(format t "mouse-move ~s: ~s,~s +~s,~s~%" w x y dx dy)
  )

(defmethod on-button ((w test1) pressed button)
  #++(map nil 'show-window
          (alexandria:hash-table-values glop2/backend-win32::*window-id-mapping*))
  (format t "button ~s: ~s,~s~%" w pressed button)
  (when pressed
    (setf (glop2:cursor w)
          (print (alexandria:random-elt '(:arrow
                                          :hourglass
                                          :arrow-hourglass
                                          :help
                                          :cross
                                          :hand
                                          :size
                                          :size-ns
                                          :size-we
                                          :size-nesw
                                          :size-nwse
                                          :i-beam
                                          :no
                                          :up-arrow))))
    #++
    (let ((c (glop2/ffi-win32::load-cursor #++(glop2/backend-win32::module-handle
                                               (glop2::window-backend-connection
                                                glop2::*application*))
                                           (cffi:null-pointer)
                                           (print
                                            (alexandria:random-elt
                                             '(:arrow
                                               :hourglass
                                               :arrow-hourglass
                                               :help
                                               :cross
                                               :hand
                                               :size
                                               :size-ns
                                               :size-we
                                               :size-nesw
                                               :size-nwse
                                               :i-beam
                                               :no
                                               :up-arrow))))))
      (format t "cursor = ~s~%" c)
      (let ((old (glop2/ffi-win32::set-cursor c)))
        (format t "old cursor = ~s~%" old)
        #++(sleep 3)
        #++(glop2/ffi-win32::set-cursor old)))))

(defmethod on-key ((w test1) pressed code sym text)
  (format t "key ~s: ~s,~s ~s ~s~%" w pressed code sym text))

(defmethod on-mouse-wheel ((w test1) wheel delta)
  (format t "wheel ~s: ~s ~s~%" w wheel delta))

#++
(with-application (:3d-api :null)
  (let ((w (make-instance 'test1))
        (w2 (make-instance 'test1)))
    (open-window w "test1a" 512 512 :x 2120 :y 16)
    (open-window w2 "test1b" 512 512 :x 2520 :y 16)
    (show-window w)
    (show-window w2)
    (loop while (dispatch-events :blocking t)
          do (format t "loop~%") (sleep 0.1))))
#++
(with-application (:3d-api :null))
#++
(ql:quickload 'mcclim-glop2)

