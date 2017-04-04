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
               :FIVEAM
               )
  :pathname "./"
  :components ((:file "app-utils")
               (:file "cl-anagrams")))

