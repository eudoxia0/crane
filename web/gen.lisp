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

;;; Code snippets
(defun get-example (name)
  (uiop:read-file-string
   (merge-pathnames
    (parse-namestring
     (format nil
             "web/examples/~A.lisp"
             name))
    +crane-path+)))

(defun make-example (name desc usage-name)
  (list :id usage-name
        :name name
        :desc desc
        :code (get-example usage-name)))

(defparameter +usage+
  (list
   (make-example "Getting it"
                 "Crane is available on <a href='http://quicklisp.org/'>Quicklisp</a>,
                  just one quick <code>quickload</code> away."
                 "obtaining")
        (make-example "Configuring and Connecting"
                      ""
                      "config")
        (make-example "Defining Tables"
                      ""
                      "table-def")
        (make-example "Creating, Saving, and Deleting Objects"
                      ""
                      "objects")
        (make-example "High-Level Interface"
                      "Simple things should be simple, and the interface is no
exception. The majority of database queries in your application will probably be
simple <code>filter</code> filter calls."
                      "high-level")
        (make-example "<a href='https://github.com/fukamachi/sxql'>SxQL</a>: Functional, Composable SQL"
                      "Most ORMs provide a single interface that looks simple
enough in the examples &mdash; but quickly becomes painful to use in real-world
cases. The Django ORM tries to get around this by adding various extensions to
its basic <code>filter</code> method that allow it to express more complicated
queries, but this rapidly becomes cumbersome. Crane's philosophy is: <strong>If
it's more complicated than a <code>filter</code>, use the SQL DSL.</strong>"
                      "sxql")
        (make-example "Transactions"
                      ""
                      "transactions")
        (make-example "Fixtures"
                      ""
                      "fixtures")))

(save (execute-emb "index"
                   :env
                   (list :title "Crane: An ORM for Common Lisp"
                         :snippet (get-example "snippet")
                         :usage +usage+))
      +index-path+)
