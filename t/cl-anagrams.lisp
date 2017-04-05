(in-package :cl-user)
(defpackage cl-anagrams-test
  (:use :cl
        :prove
        :cl-anagrams))

(in-package :cl-anagrams-test)

(plan nil)


;;; tests assume the default wordlist.
(is (= (length *wordlist*) 416111))
(is (= (length (rutils:hash-table-keys *anagrams*)) 357027))
(is (= (length (return-valid-anagrams *anagrams*)) 37775))

(finalize)
