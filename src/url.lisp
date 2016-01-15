(in-package :cl-user)
(defpackage crane.url
  (:use :cl)
  (:import-from :alexandria
                :switch)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :crane.database.postgres
                :postgres)
  (:import-from :crane.database.mysql
	        :mysql)
  (:import-from :crane.database.sqlite3
                :sqlite3)
  (:export :parse)
  (:documentation "Parse SQLAlchemy-like database URLs."))
(in-package :crane.url)

(defun parse (url)
  "Parse a database URL, returning a database object or @c(nil)."
  (declare (type string url))
  (let* ((uri (quri:uri url))
         (class (switch ((quri:uri-scheme uri) :test #'string=)
                  ("postgres"
                   'postgres)
                  ("mysql"
                   'mysql)
                  ("sqlite3"
                   'sqlite3)
                  (t
                   (error "Unsupported database: ~A" (quri:uri-scheme uri)))))
         (user-pass (if (quri:uri-userinfo uri)
                        (split-sequence #\: (quri:uri-userinfo uri))
                        nil))
         (username (first user-pass))
         (password (second user-pass))
         (host (quri:uri-domain uri))
         (port (quri:uri-port uri))
         (name (subseq (quri:uri-path uri) 1)))
    (ccase class
      (postgres
       (make-instance 'postgres
                      :name name
                      :username username
                      :password password
                      :host (or host crane.database.postgres:*default-host*)
                      :port (or port crane.database.postgres:*default-port*)))
      (mysql
       (make-instance 'mysql
                      :name name
                      :username username
                      :password password
                      :host (or host crane.database.mysql:*default-host*)
                      :port (or port crane.database.mysql:*default-port*)))
      (sqlite3
       (make-instance 'sqlite3
                      :name (let ((pathname (parse-namestring
                                             (format nil "~A/~A" host name))))
                              (namestring
                               (if (uiop:absolute-pathname-p pathname)
                                   pathname
                                   ;; If the path is relative, merge it with
                                   ;; *default-pathname-defaults*
                                   (uiop:truenamize pathname)))))))))
