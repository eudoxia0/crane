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
  (when (and (crane.utils:debugp) args)
    (print args))
  (apply #'dbi:execute (cons query args)))

@doc "A combination of EXECUTE and SxQL's YIELD."
@export
(defmacro query (body &optional (database-name crane:*default-db*))
  `(multiple-value-bind (sql args) (sxql:yield ,body)
     (let ((result (apply #'execute
                          (cons (prepare sql ,database-name) args))))
       (when result (dbi:fetch-all result)))))

(defun latest-id (class)
  (let ((result (getf (car (query
                               (sxql:select ((:max :id))
                                 (sxql:from (table-name class)))
                               (db class)))
                      :|max|)))
    (if (eql result :null) 0 result)))
