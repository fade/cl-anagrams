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
;; :*anagrams*
;; :print-anagrams-as-text
;; :print-anagram-dictionary
;; :return-valid-anagrams
;; :output-file-of-anagrams
;; :lookup-word

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

(finalize)
