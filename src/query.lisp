(defpackage :crane.query
  (:use :cl :anaphora :iter :cl-annot.doc)
  (:import-from :crane.meta
                :table-name
                :db)
  (:import-from :crane.config
                :debugp))
(in-package :crane.query)
(annot:enable-annot-syntax)

@doc "A combination of EXECUTE and SxQL's YIELD."
@export
(defmacro query (body &optional database-name)
  `(multiple-value-bind (sql args) (sxql:yield ,body)
     (when (crane.config:debugp)
       (format t "~&Query: ~A~&" sql))
     (let* ((prepared (dbi:prepare (crane.connect:get-connection ,database-name)
                                   sql))
            (result (apply #'dbi:execute
                           (cons prepared args))))
       (when result (dbi:fetch-all result)))))
