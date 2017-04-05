(in-package :cl-user)
(defpackage cl-anagrams-test
  (:use :cl
        :prove
        :cl-anagrams))

(in-package :cl-anagrams-test)



;;; tests assume the default wordlist.
(plan nil)

(ok (= (length *wordlist*) 416111))
(ok (= (length (rutils:hash-table-keys *anagrams*)) 357027))
(ok (= (length (return-valid-anagrams *anagrams*)) 37775))

(finalize)
