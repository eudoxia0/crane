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
                      "First things first: Setup. All Crane needs to get going
is a list of databases to connect to, authentication information, and a
directory where it will store migrations."
                      "config")
        (make-example "Defining Tables"
                      "Crane has a very simple table definition syntax, inspired
by that of the Django ORM. This, combined with automatic migrations, lets you
rapidly prototype and experiment with table definitions."
                      "table-def")
        (make-example "Automatic Migrations"
                      "Migrations are just another part of your development
cycle &mdash; not an exceptional situation. Just change your table definitions
and let Crane figure things out. You don't even have to leave your editor to run
a command, locally or in a remote server."
                      "migrations")
        (make-example "Creating, Saving, and Deleting Objects"
                      ""
                      "objects")
        (make-example "High-Level Interface"
                      "Simple things should be simple, and the interface is no
exception. The majority of database queries in your application will probably be
simple <code>filter</code> filter calls."
                      "high-level")
        (make-example "<a href='https://github.com/fukamachi/sxql'>SxQL</a>: Functional, Composable SQL"
                      "Most ORMs provide a simple interface that looks simple
enough in the examples &mdash; but quickly becomes painful to use in real-world
cases. The Django ORM tries to get around this by adding various extensions to
its basic <code>filter</code> method that allow it to express more complicated
queries, but this rapidly becomes cumbersome. Crane's philosophy is: <strong>If
it's more complicated than a <code>filter</code>, use the SQL DSL.</strong>"
                      "sxql")
        (make-example "Transactions"
                      "Crane provides a macro &ndash;
<code>with-transaction</code> &ndash; that automatically takes care of setting
up a transaction and aborting when conditions (Exceptions) are signalled. A
manual interface is also provided to provide more fine-grained control over your
transactions."
                      "transactions")
        (make-example "Fixtures"
                      "Fixtures are provided through the <a
href='https://github.com/eudoxia0/clos-fixtures'>clos-fixtures</a> library, and
can be used for anything from loading mostly unchanging data (A list of
countries, for example) to setting up massive datasets for testing."
                      "fixtures")))

(save (execute-emb "index"
                   :env
                   (list :title "Crane: An ORM for Common Lisp"
                         :snippet (get-example "snippet")
                         :usage +usage+))
      +index-path+)
