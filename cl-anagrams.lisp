(defpackage :cl-anagrams
            (:use :cl)
            (:nicknames "ANAGRAMS")
            ;; (:use #:alexandria #:rutils)
            (:use #:cl-anagrams.app-utils
                  #:net.didierverna.clon)
            (:export :-main))

(in-package :cl-anagrams)

;;; static system state
(defvar *base-pathname*
  #.(uiop:pathname-directory-pathname (or *compile-file-truename* *load-truename*))
  "we need this value to be calculated at run-time, not fixed at compile time.")

;; (format t "~&The path is the way and the way is ~A" *base-pathname*)

(defun uniquify (wordlist)
  "we're misusing a hashtable here for the hash function's feature of flattening
duplicate values in its keyspace very quickly. By creating it equal to the
length of the input, we ensure that the table never has to be rehashed while it
is filled."
  (let ((col (make-hash-table :size (length wordlist) :test #'equal)))
    (loop for word in wordlist
          do (setf (gethash word col) nil)
          finally (return (nreverse (alexandria:hash-table-keys col))))))

(defun telebonk (&optional (wordlist *wordlist*))
  "Return a list of lists of each anagram."
  (loop for word in wordlist
        for targs = (lookup-word word)
        if targs
          :collect targs))

(defun emit-anagrams (&optional (stream t) (wordlist *wordlist*))
  "Write anagrams as lines of comma separated values to <stream>. Defaults to
stdout."
  (loop for word in wordlist
        for ana = (lookup-word word)
        if ana
          do (format stream "~{~A~^, ~}~%" ana)))

(defun emit-wordlist (&key (stream t) (wordlist *wordlist*))
  "write the wordlist to <stream> as comma separated values. Defaults to
  stdout"
  (loop for count from 1 to (length wordlist)
        for word in wordlist
        when (= (mod count 9) 0)
          do (format stream "~A, ~%" word)
        else
          do (format stream "~A, " word))
  (finish-output))

(defun dump-wordlist (filename &key (wordlist *wordlist*))
  "Given a pathname and an optional list of words (defaults to *wordlist*),
write the words to the pathname separated by commas."
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (emit-wordlist :stream s :wordlist wordlist)))

(defun read-clean-words (path)
  (let* ((tmpwords (rutils:split-string
                    (alexandria:read-file-into-string
                     path)))
         (testwords (loop for word in tmpwords 
                          if (every #'alpha-char-p word)
                            collect (string-downcase word) into ham
                          finally (return ham))))
    (uniquify testwords)))

(defparameter *wordlist* 
  (sort (read-clean-words
         (merge-pathnames "constants/bonk.list" *base-pathname*)) #'string<))

(defun normalise-word (word)
  (sort (string-downcase word) #'char<))

(defun build-anagram-hash-table (agram-t wordlist)
  "intern anagrams in a normalised hash-table."
  (loop for word in wordlist
        for normal = (sort (string-downcase word) #'char<)
        do (multiple-value-bind (k p) (gethash normal agram-t)
             (declare (ignorable k))
             (if p
                 (pushnew word (gethash normal agram-t))
                 (setf (gethash normal agram-t) (list word))))))

(defvar *anagrams* (make-hash-table :test #'equal))

(build-anagram-hash-table *anagrams* *wordlist*)

(defun print-anagrams-as-text (&optional (anagram-t *anagrams*))
  "print to standard output a comma separated list of each anagram tuple we have
recorded, with tuples separated by newlines."
  (loop for words in (return-valid-anagrams anagram-t)
        do (format t "~&~{~A~^, ~}" words)))

(defun print-anagram-dictionary (agram-t &optional (stream t))
  (loop for k being the hash-keys in agram-t using (hash-value v)
        if (> (length v) 1)
          counting k
          do (format stream "~&~A || ~S" k v)))

(defun return-valid-anagrams (agram-t)
  (loop for k being the hash-keys in agram-t using (hash-value v)
        if (> (length v) 1)
          collect v into stuff
        finally (return stuff)))

(defun output-file-of-anagrams (agram-t &optional (filename #P "/tmp/anagrams.txt"))
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (let ((words agram-t))
      (loop for slice in words
            do (format s "~&~A || ~{~A~^, ~}" (first slice) (rest slice))))))

(defun lookup-word (word &optional (agram-t *anagrams*) (test nil))
  "given a word and a hash table of anagrams, return any anagrams for the given
word contained in the built in dictionary."
  (let ((normalized-word (sort (string-downcase word) #'char<)))
    (multiple-value-bind (k p) (gethash normalized-word agram-t)
      ;; (format t "[[~{~A~^ ~}]]" k)
      (cond ((and p (> (length k) 1))
             (if test
                 (format t "~&~{~A~^ ~}~%" k))
             k)
            (t nil)))))


;; CLON setup
(defsynopsis (:postfix "WORDS ...")
  (text :contents "A tool for anagram discovery. Base dictionary preloaded")
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version number and exit."))
  (group (:header "Managing working dictionary")
         (flag :short-name "d" :long-name "dict"
               :description "Load a custom dictionary on top of the built in wordlist.")
         (flag :short-name "w" :long-name "wfile"
               :description "Dump the wordlist to a comma separated file.
               Defaults to /tmp/wordlist.txt")
         (flag :short-name "j" :long-name "wjson"
               :description "Dump the wordlist as a JSON formatted string to
               stdout.")))


(defun -main (argv)
  "Entry point for the anagram discovery tool."
  (make-context)
  (format t "~& ~D ARGV :: ~{~A~^ ~}~%" (length argv) argv)
  (let* ((word (second argv))
         (anagrams (lookup-word word)))
    (format t "~&~A has ~D, which are~%~{~A~^, ~}~%" word (length anagrams) anagrams)))


