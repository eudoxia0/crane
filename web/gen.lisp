(defpackage :crane.web
  (:use :cl)
  (:import-from :trivial-download
                :download))
(in-package :crane.web)

(defparameter +crane-path+
  (asdf:component-pathname (asdf:find-system :crane)))

(defparameter +zip-path+
  (merge-pathnames
   #p"highlight-lisp.zip"
   +crane-path+))

(unless (probe-file +zip-path+)
  (download "https://github.com/orthecreedence/highlight-lisp/archive/master.zip"
            +zip-path+)
  (zip:unzip +zip-path+
             (merge-pathnames
              #p"highlight-lisp/"
              +crane-path+)))

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
