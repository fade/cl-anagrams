;; (defpackage :cl-anagrams.app-utils
;;   (:use :cl
;;    :cl-json)
;;   (:export :internal-disable-debugger)
;;   (:export :internal-quit))

(in-package :cl-anagrams.app-utils)
  
(defun internal-disable-debugger ()
  (labels
      ((internal-exit (c h)
             (declare (ignore h))
             (format t "~a~%" c)
             (internal-quit)))
    (setf *debugger-hook* #'internal-exit)))

(defun internal-quit (&optional code)
  (uiop:quit code))
