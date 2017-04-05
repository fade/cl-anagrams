(in-package :cl-user)
(defpackage cl-anagrams-test
  (:use :cl
        :prove
        :cl-anagrams))

(in-package :cl-anagrams-test)

(plan nil)

;; (ok (not (find 4 '(1 2 3))))
;; (is 4 4)
;; (isnt 1 #\1)

(ok (= (length *wordlist*) 416111))
(ok (= (hash-table-size *anagrams*) 524288))
(ok (= (length (return-valid-anagrams *anagrams*)) 37775))

(finalize)
