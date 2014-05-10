(defpackage :crane.web
  (:use :cl :cl-markup))
(in-package :crane.web)

(defparameter +index-path+
  (merge-pathnames
   #p"index.html"
   (asdf:component-pathname (asdf:find-system :crane))))

(defparameter *css*
  (list "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
        "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"))
(defparameter *js*
  (list "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"))

(defmacro save (&rest body)
  `(with-open-file (stream +index-path+
                           :direction :output
                           :if-exists :supersede)
     (write-string (html5 ,@body) stream)))

(save
 (:head
  (:title "Crane: A Common Lisp ORM")
  (loop for uri in *css* collecting
        (markup (:link :rel "stylesheet" :href uri)))
  (loop for uri in *js* collecting
        (markup (:script :src uri ""))))
 (:body
  "The body should be here, but I have no creativity"))
