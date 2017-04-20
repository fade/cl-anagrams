(in-package :cl-anagrams.web)

(unless (eq (html-mode) :html5)
  (setf (html-mode) :html5))

(defmacro standard-page ((&key title) &body body)
  "This macro emits a 'standard' page, with appropriate prologue, header, and
footer. It takes a body of cl-who keyword forms defining the actual HTML to
emit."
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "static/anagrams.css"))
            (:body
             (:div :id "header" ; Retro games header
                   (:img :src "static/logo.png"
                         :alt "logo image"
                         :class "logo")
                   (:span :class "strapline"
                          "Anagrams are interesting!"))
             ,@body
             (:div :id "footer"
                   :align "right"
                   (:img :src "img/made-with-lisp-logo.png"))))))

(defun top-page ()
  (standard-page (:title "Anagram Explorer")
    (:h1 "Explore a wide array of Anagrams...")
    (:p "Write this code in the future.")))

;;;; Setup the page handlers for the application

;; (define-easy-handler (anagrams :uri "/") ()
;;   (top-page))

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

;; (define-easy-handler (word :uri "/lookup" ())
;;     (standard-page))

(define-easy-handler (lookup :uri "/") ()
  (standard-page (:title "Lookup up any potential anagrams of a word")
    (:h1 "Lookup anagrams of a given word")
    (:form :action "/lword" :method "post" :id "lookup"
           (:p "What is the word?" (:br)
               (:input :type "text" :name "word" :class "txt"))
           (:p (:input :type "submit" :value "Lookup" :class "btn")))))

(define-easy-handler (lword :uri "/lword") (word)
  (unless (or (null word) (zerop (length word)))
    (let ((glist (lookup-word word)))
      (format t "~{~A~^, ~}" glist)
      (standard-page
          (:title "Anagram results")
        (:h1 (if glist
                 (str (format nil "~S is in the set of the following anagrams:" word))
                 (str (format nil "~s does not have any anagrams." word))))
        (:div :id "chart"
              (:ol
               (dolist (w glist)
                 (htm
                  (:li (str w))))))
        (:br)
        (:a :href "/" (:b (str "Again?")))))))

;;;; start and stop the server


(defparameter *anagram-server* nil)

(defun start-anagrams (&key (port 8080))
  "start the server and bind it to a handle for later reference."
  (setf *anagram-server*
        (start
         (make-instance 'hunchentoot:easy-acceptor
                        :port port
                        :document-root (merge-pathnames "www/" cl-anagrams:*base-pathname*)
                        :error-template-directory (merge-pathnames "www/errors/" cl-anagrams:*base-pathname*)))))

(defun stop-anagrams ()
  "stop with the word salad already."
  (stop *anagram-server*)
  (setf *anagram-server* nil))





