(defpackage :crane.web
  (:use :cl))
(in-package :crane.web)

(defparameter +index-path+
  (merge-pathnames
   #p"index.html"
   (asdf:component-pathname (asdf:find-system :crane))))

(defmacro save (string)
  `(with-open-file (stream +index-path+
                           :direction :output
                           :if-exists :supersede)
     (write-string ,string stream)))
