;;;; cl-anagrams.asd

(asdf:defsystem #:cl-anagrams
  :description "a utility to deal with anagrams"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "Modified BSD License"
  :serial t
  :in-order-to ((test-op (test-op cl-anagrams-test)))
  :depends-on (:alexandria
               :rutils
               :inferior-shell
               :fare-csv
               :net.didierverna.clon
               :hunchentoot
               :parenscript
               :cl-css
               :cl-who
               :cl-json
               :drakma
               :swank)
  ;; :pathname "./"
  :components ((:file "packages")
               (:file "app-utils" :depends-on ("packages"))
               (:file "random-toy-functions" :depends-on ("packages"))
               (:file "web" :depends-on ("packages"))
               (:file "cl-anagrams" :depends-on ("packages"))))


(defpackage :cl-anagrams.app-utils
  (:use :cl)
  (:export :internal-disable-debugger)
  (:export :internal-quit))

