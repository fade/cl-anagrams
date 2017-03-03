(defpackage :cl-anagrams
            (:use :cl)
            (:use :cl-anagrams.app-utils)
            (:export :-main))

(in-package :cl-anagrams)

(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

