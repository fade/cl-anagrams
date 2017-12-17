(in-package :cl-anagrams)

;;; these were largely used for development testing.
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
;;; /development testing.


;;; slim down commandline option setup; unused for pedagogical reasons.
(defmacro standard-opts-with-exit (s l &body body)
  `(or (string= ,s) (string= ,l)
       ,@body
       (uiop:quit 0)))


;;; portable and sbcl specific thread management functions

(defun blag (p)
  (let ((port p))
    (find-if
     (lambda (th)
       (string= (bordeaux-threads:thread-name th)
                (format nil "hunchentoot-listener-*:~A" port)))
     (bordeaux-threads:all-threads))))

#+SBCL
(defun boog (p)
  (let ((port p))
    (find-if
     (lambda (th)
       (string= (sb-thread:thread-name th)
                (format nil "hunchentoot-listener-*:~A" port)))
     (sb-thread:list-all-threads))))
