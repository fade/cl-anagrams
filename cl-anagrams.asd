;;;; cl-anagrams.asd

(asdf:defsystem #:cl-anagrams
  :description "a utility to deal with anagrams"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Modified BSD License"
  :serial t
  :in-order-to ((test-op (test-op cl-anagrams-test)))
  :depends-on (:ALEXANDRIA
               :RUTILS
               :FARE-CSV
               :NET.DIDIERVERNA.CLON
               :HUNCHENTOOT
               :PARENSCRIPT
               :CL-WHO)
  ;; :pathname "./"
  :components ((:file "packages")
               (:file "app-utils" :depends-on ("packages"))
               (:file "web" :depends-on ("packages"))
               (:file "cl-anagrams" :depends-on ("packages"))))


(defpackage :cl-anagrams.app-utils
  (:use :cl)
  (:export :internal-disable-debugger)
  (:export :internal-quit))

