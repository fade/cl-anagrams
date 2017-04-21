;; (defpackage :cl-anagrams.app-utils
;;   (:use :cl
;;    :cl-json)
;;   (:export :internal-disable-debugger)
;;   (:export :internal-quit))

(in-package :cl-anagrams.app-utils)

(defun strip-string (string)
  (string-trim " ^M^Z" string))

(defun strip-spaces (string-list)
  (mapcar #'strip-string string-list))


(defun internal-disable-debugger ()
  (labels
      ((internal-exit (c h)
             (declare (ignore h))
             (format t "~a~%" c)
             (internal-quit)))
    (setf *debugger-hook* #'internal-exit)))

(defun internal-quit (&optional code)
  (uiop:quit code))
