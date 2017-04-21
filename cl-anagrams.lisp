
(in-package :cl-anagrams)

;;; static system state
(defvar *base-pathname*
  #.(uiop:pathname-directory-pathname (or *compile-file-truename* *load-truename*))
  "we need this value to be calculated at run-time, not fixed at compile time.")

;;; output our metadata in various ways.

(defun telebonk (&optional (wordlist *wordlist*))
  "Return a list of lists of each anagram."
  (princ "HELLO FROM EMACS!")
  (terpri)
  (loop for word in wordlist
        for targs = (lookup-word word)
        if targs
          :collect targs))

(defun emit-anagrams (&key (stream t) (wordlist *wordlist*))
  "Write anagrams as lines of comma separated values to <stream>. Defaults to
stdout."
  (loop for word in wordlist
        for ana = (lookup-word word)
        if (and ana (> (length ana) 1))
          do (format stream "~{~A~^, ~}~%" ana)))

(defun emit-wordlist (&key (stream t) (wordlist *wordlist*))
  "write the wordlist to <stream> as comma separated values. Defaults to
  stdout"
  (loop for count from 1 to (length wordlist)
        for word in wordlist
        when (= (mod count 9) 0)
          do (format stream "~A,~%" word)
        else
          do (format stream "~A, " word))
  (finish-output))

(defun dump-wordlist (filename &key (wordlist *wordlist*))
  "Given a pathname and an optional list of words (defaults to *wordlist*),
write the words to the pathname separated by commas (CSV)."
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (emit-wordlist :stream s :wordlist wordlist)))

(defun print-anagrams-as-text (&optional (anagram-t *anagrams*))
  "print to standard output a comma separated list of each anagram tuple we have
recorded, with tuples separated by newlines."
  (loop for words in (return-valid-anagrams anagram-t)
        do (format t "~&~{~A~^, ~}" words)))

;;; /metadata

(defun uniquify (wordlist)
  "we're misusing a hashtable here for the hash function's feature of flattening
duplicate values in its keyspace very quickly. By creating it equal to the
length of the input, we ensure that the table never has to be rehashed while it
is filled."
  (let ((col (make-hash-table :size (length wordlist) :test #'equal)))
    (loop for word in wordlist
          do (setf (gethash word col) nil)
          finally (return (nreverse (alexandria:hash-table-keys col))))))

(defun read-clean-words (path)
  "read in a file of words from <path>."
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
  "return word as a string downcased and sorted alphabetically."
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

;;; all side effects, all the time. After this point, we have precalculated all the
;;; anagrams that are contained in our wordlist.
(build-anagram-hash-table *anagrams* *wordlist*)



;;; return the largest anagram set in our data
(defun largest-anagram-set (&optional (anagrams *anagrams*))
  "return the largest set of anagrams in the hashtable passed in as 'anagrams."
  (let* ((anas (return-valid-anagrams anagrams))
         (maxlen (loop for x in anas maximize (list-length x))))
    (find maxlen anas :key #'list-length :test 'equalp)))

(defun anagram-frequencies-for-n (n anagrams)
  ""
  (let* ((anas (return-valid-anagrams anagrams)))
    (loop for x in anas
          when (= (length x) n)
            collect x)))

(defun anagram-frequency-map (anagrams)
  (loop for i from 1 to 25
        for anas = (anagram-frequencies-for-n i anagrams)
        if anas
          collect (cons i (list anas))))

;;; this is the primary entry to the angram database.
(defun lookup-word (word &optional (agram-t *anagrams*) (test nil))
  "given a word and a hash table of anagrams, return any anagrams for the given
word."
  (let ((normalized-word
          (strip-string
           (substitute-if-not #\Space #'alpha-char-p
                              (sort (string-downcase word) #'char<)))))
    (multiple-value-bind (k p) (gethash normalized-word agram-t)
      ;; (format t "[[~{~A~^ ~}]]" k)
      (cond ((and p (> (length k) 1))
             (if test
                 (format t "~&~{~A~^ ~}~%" k)
                 k))
            (t nil)))))

;; CLON setup
(defsynopsis (:postfix "Word..")
  (text :contents "A tool for anagram discovery. Base dictionary preloaded, and
  resident.")
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version number and exit."))
  (group (:header "Managing working dictionary")
         ;; (flag :short-name "s" :long-name "stdout"
         ;;       :description "Force dictionary commands to output to standard out")
         ;; (flag :short-name "l" :long-name "localhost"
         ;;       :description "Bind the webserver to localhost.")
         (stropt :short-name "t" :long-name "http"
                 :description "Start up a webserver for anagram requests on the supplied port."
                 :default-value "8888")
         (stropt :short-name "d" :long-name "dict"
                 :description "Load a custom dictionary on top of the built in wordlist."
                 :argument-name "DICT"
                 :default-value "static/bonk.list")
         (stropt :short-name "w" :long-name "wfile"
                 :description "Dump the wordlist to a comma separated file.
                 Defaults to /tmp/wordlist.txt"
                 :argument-name "WLIST"
                 :default-value "/tmp/wordlist.list")
         (stropt :short-name "a" :long-name "anagrams"
                 ;; :argument-type "ANAGRAMS"
                 :default-value "/tmp/anagrams.csv")
         (stropt :short-name "j" :long-name "wjson"
                 :description "Dump the wordlist as a JSON formatted string to file."
                 :argument-name "JSON"
                 :default-value "/tmp/anagrams.json")))

(defun -main (argv)
  "Entry point for the anagram discovery tool."
  (make-context)
  (defparameter *anagram-server* nil)
  ;; (format t "~& ~D ARGV :: ~{~A~^ ~}~%" (length argv) argv)
  (net.didierverna.clon:do-cmdline-options (option name value source)
    (cond ((or (string= name "h") (string= name "help"))
           (terpri)
           (help)
           (terpri)
           (uiop:quit 0))
          
          ((or (string= name "v") (string= name "version"))
           (terpri)
           (format t "~A~%" "0.0.1")
           (uiop:quit 0))

          ((or (string= name "t") (string= name "http"))
           (setf swank::*loopback-interface* "127.0.0.1")
           (swank:create-server :port 4007 :style :spawn :dont-close t )
           (let ((port (parse-integer value :junk-allowed t)))
             (setf *anagram-server*  (cl-anagrams.web:start-anagrams :port port))
             ;; we don't want the web server to exit immediately, so join it to
             ;; this execution context
             #+sbcl
             (sb-thread:join-thread (find-if
                                     (lambda (th)
                                       (string= (sb-thread:thread-name th)
                                                (format nil "hunchentoot-listener-*:~A" port)))
                                     (sb-thread:list-all-threads)))
             #+ccl
             (bordeaux-threads:join-thread (find-if
                                            (lambda (th)
                                              (string= (bordeaux-threads:thread-name th)
                                                       (format nil "hunchentoot-listener-*:~A" port)))
                                            (bordeaux-threads:all-threads)))))
          
          ((or (string= name "d") (string= name "dict")))
          
          ((or (string= name "w") (string= name "wfile"))
           (with-open-file (f value :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
             (emit-wordlist :stream f)
             (format t "~&File output to: ~A~%" value)))
          
          ((or (string= name "a") (string= name "anagrams"))
           (with-open-file (f value :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
             (emit-anagrams :stream f)
             (format t "~&File output to: ~A~%" value)))
          ((or (string= name "j") (string= name "wjson"))
           (with-open-file (f value :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
             (cl-json:encode-json *anagrams* f)))
          
          (t
           (format t "~&~A has ~D, which are~%~{~A~^, ~}~%" word (length anagrams) anagrams)))))


