(defpackage :crane.web
  (:use :cl)
  (:import-from :trivial-download
                :download)
  (:import-from :cl-emb
                :register-emb
                :execute-emb))
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

(register-emb "head" (merge-pathnames
                       #p"web/templates/head.tmpl"
                       +crane-path+))
(register-emb "snippet" (merge-pathnames
                       #p"web/examples/snippet.lisp"
                       +crane-path+))
(register-emb "config" (merge-pathnames
                       #p"web/examples/config.lisp"
                       +crane-path+))
(register-emb "table-def" (merge-pathnames
                       #p"web/examples/table-def.lisp"
                       +crane-path+))
(register-emb "objects" (merge-pathnames
                       #p"web/examples/objects.lisp"
                       +crane-path+))
(register-emb "features" (merge-pathnames
                          #p"web/templates/features.tmpl"
                          +crane-path+))
(register-emb "index" (merge-pathnames
                       #p"web/templates/index.tmpl"
                       +crane-path+))


(save (execute-emb "index" :env '(:title "Crane: An ORM for Common Lisp"))
      +index-path+)
