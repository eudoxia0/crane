(in-package :crane)
(annot:enable-annot-syntax)

@doc "A map from database names to connections."
(defparameter *db* (make-hash-table))

(defun connect-spec (spec)
  nil)

@doc "Connect to all the databases specified in the configuration."
(defun connect ()
  (aif (get-config-value :databases)
       (iter (for (db spec) on it by #'cddr)
             (aif (gethash db *db*)
                  (error 'crane.errors:configuration-error
                         :key :databases
                         :text "Two databases have the same name.")
                  (setf it (connect-spec spec))))
       (error 'crane.errors:configuration-error
              :key :databases
              :text "No databases found.")))
