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

;;; Code snippets
(defun get-example (name)
  (uiop:read-file-string
   (merge-pathnames
    (parse-namestring
     (format nil
             "web/examples/~A.lisp"
             name))
    +crane-path+)))

;;; Templates
(register-emb "head" (merge-pathnames
                       #p"web/templates/head.tmpl"
                       +crane-path+))
(register-emb "features" (merge-pathnames
                          #p"web/templates/features.tmpl"
                          +crane-path+))
(register-emb "index" (merge-pathnames
                       #p"web/templates/index.tmpl"
                       +crane-path+))

(defparameter +usage+
  (list (list :name "Configuring and Connecting"
              :code (get-example "config"))
        (list :name "Defining Tables"
              :code (get-example "table-def"))
        (list :name "Creating, Saving, and Deleting Objects"
              :code (get-example "objects"))
        (list :name "High-Level Interface"
              :code (get-example "high-level"))
        (list :name "SxQL: Functional, Composable SQL")
        (list :name "Transactions"
              :code (get-example "transactions"))
        (list :name "Fixtures"
              :code (get-example "fixtures"))))

(save (execute-emb "index"
                   :env
                   (list :title "Crane: An ORM for Common Lisp"
                         :snippet (get-example "snippet")
                         :usage +usage+))
      +index-path+)
