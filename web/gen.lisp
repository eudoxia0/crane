(defpackage :crane.web
  (:use :cl :cl-markup))
(in-package :crane.web)

(defparameter +index-path+
  (merge-pathnames
   #p"index.html"
   (asdf:component-pathname (asdf:find-system :crane))))

(defparameter *css*
  (list "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
        "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"
        "http//cdn.jsdelivr.net/typeplate/1.1.2/typeplate.min.css"
        "web/style.css"))

(defparameter *js*
  (list "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"))

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
  (:h1 "Crane")))
