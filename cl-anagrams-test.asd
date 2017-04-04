(defsystem "cl-anagrams-test"
  :author "Brian O'Reilly"
  :license "Modified BSD License"
  :depends-on (:prove
               :alexandria
               :fare-csv
               :rutils
               :cl-anagrams)
  :components ((:module "t"
                        :serial t
                        :components
                        ((:test-file "cl-anagrams"))))
  :description "Test system for cl-anagrams."
  ;; this allows the prove test syntax in defsystem.
  :defsystem-depends-on (:prove-asdf))

