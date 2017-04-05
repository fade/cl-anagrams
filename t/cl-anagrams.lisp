(in-package :cl-user)
(defpackage cl-anagrams-test
  (:use :cl
        :prove
        :cl-anagrams))

(in-package :cl-anagrams-test)



;;; tests assume the default wordlist.
(plan nil)

(is (length *wordlist*) 416111)
(is (length (rutils:hash-table-keys *anagrams*)) 357027)
(is (length (return-valid-anagrams *anagrams*)) 37775o)

(finalize)
