(defpackage crane.query
  (:use :cl :anaphora :iter)
  (:import-from :crane.meta
                :table-name
                :db)
  (:import-from :crane.config
                :debugp)
  (:export :meta-query
           :query
           :do-query)
  (:documentation "Executing cl-dbi queries in the context of Crane."))
(in-package :crane.query)

(defmacro meta-query (query database-name body)
  `(multiple-value-bind (sql args) (sxql:yield ,query)
     (when (crane.config:debugp)
       (format t "~&Query: ~A~&" sql))
     (let* ((prepared (dbi:prepare (crane.connect:get-connection ,database-name)
                                   sql))
            (result (apply #'dbi:execute
                           (cons prepared args))))
       (when result ,body))))

(defmacro query (query &optional database-name)
  "Execute an SxQL query on the database `database-name`."
  `(crane.query:meta-query ,query ,database-name (dbi:fetch-all result)))

(defmacro do-query ((result-name query &optional database-name)
                    &rest body)
  "Execute code for each result in the query, without aggregating them all into
a list."
  `(crane.query:meta-query ,query ,database-name
                           (loop for ,result-name = (dbi:fetch result)
                                 while ,result-name do
                                 ,@body)))
