(defpackage :crane.web
  (:use :cl))
(in-package :crane.web)

(defparameter +index-path+
  (merge-pathnames
   #p"index.html"
   (asdf:component-pathname (asdf:find-system :crane))))

(defmacro save (string path)
  `(with-open-file (stream ,path
                           :direction :output
                           :if-exists :supersede)
     (write-string ,string stream)))

(save (view:index) +index-path+)
