;;;; cl-anagrams.asd

(asdf:defsystem #:cl-anagrams
                :description "a utility to deal with anagrams"
                :author "Brian O'Reilly <fade@deepsky.com"
                :license "Modified BSD License"
                :serial t
                :depends-on (:ALEXANDRIA
:RUTILS
:CL-PPCRE
:CHANL
:FIVEAM
)
                :pathname "./"
                :components ((:file "app-utils")
                             (:file "cl-anagrams")
                             ))

