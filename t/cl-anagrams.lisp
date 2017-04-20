(in-package :cl-user)
(defpackage cl-anagrams-test
  (:use :cl
        :prove
        :cl-anagrams
        :uiop))

(in-package :cl-anagrams-test)

;;;; the functions and names exported from cl-anagrams:

;; :-main
;; :*base-pathname*
;; :uniquify
;; :emit-anagrams
;; :emit-wordlist
;; :dump-wordlist
;; :read-clean-words
;; :*wordlist*
;; :normalise-word
;; :build-anagram-hash-table
;x; :*anagrams*
;x; :return-valid-anagrams
;; :output-file-of-anagrams
;x; :lookup-word

;;; tests assume the default wordlist.
(plan nil)

;1; check the default wordlist has been appropriately parsed
(ok (= (length *wordlist*) 416111))

;2; check that the table of anagrams contains the expected keys given the
;;; default wordlist.
(ok (= (length (rutils:hash-table-keys *anagrams*)) 357027))

;3; check that the valid anagrams inside the table contains the expected words
;given the default wordlist.
(ok (= (length (return-valid-anagrams *anagrams*)) 37775))

;; ;4; check that lookup-word is returning expected results
;; (ok (= 18 (+ (length (lookup-word "poesis")) (length (lookup-word "heart")))))

;; ;5; known anagrams in our hashtable.
;; (ok (and (find "thera" (lookup-word "heart") :test #'string=)
;;          (find "posies" (lookup-word "poesis" :test #'string=))))

(finalize)
