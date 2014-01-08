(in-package :crane)
(annot:enable-annot-syntax)

@doc "Prepare a query for execution"
@export
(defun prepare (query &optional (database-name crane:*default-db*))
  (when (crane.utils:debugp)
    (print query))
  (dbi:prepare (crane:get-connection database-name) query))

@doc "Execute a query."
@export
(defun execute (query &rest args)
  (apply #'dbi:execute (cons query args)))

@doc "A combination of EXECUTE and SxQL's YIELD."
@export
(defmacro query (body &optional (database-name crane:*default-db*))
  `(multiple-value-bind (sql args) (sxql:yield ,body)
     (dbi:fetch-all (apply #'execute
                           (cons (prepare sql ,database-name) args)))))
