;;;; cl-anagrams.asd

(defpackage :cl-anagrams
            (:use :cl)
            (:nicknames "ANAGRAMS")
            ;; (:use #:alexandria #:rutils)
            (:use #:cl-anagrams.app-utils
                  ;; #:cl-anagrams.web
                  #:net.didierverna.clon)
            (:export :-main
                     :*base-pathname*
                     :uniquify
                     :emit-anagrams
                     :emit-wordlist
                     :dump-wordlist
                     :read-clean-words
                     :*wordlist*
                     :normalise-word
                     :build-anagram-hash-table
                     :*anagrams*
                     :print-anagrams-as-text
                     :print-anagram-dictionary
                     :return-valid-anagrams
                     :output-file-of-anagrams
                     :lookup-word))

(defpackage :cl-anagrams.web
  (:use :cl
        :cl-who
        :parenscript
        :hunchentoot
        :cl-anagrams)
  (:export :start-anagrams))

