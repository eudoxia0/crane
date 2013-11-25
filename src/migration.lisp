(defpackage :crane.migration
  (:use :cl :anaphora))
(in-package :crane.migration)
(annot:enable-annot-syntax)

(defun get-configuration ()
  (aif (getf (envy:config *package*) :crane)
       it
       (error 'crane.errors:no-configuration-error)))

(defun get-config-value (property)
  (aif (getf (get-configuration) property)
       it
       (error 'crane.errors:configuration-error
              :text "The configuration for the property ~A was not found."
              property)))

(defun get-migration-dir ()
  (ensure-directories-exist (get-config-value :migrations-directory)))

(defun migration-history-pathname (table-name)
  "Return the pathname to the file containing the migration
history for the table `table-name`."
  (merge-pathnames
   (make-pathname :name (symbol-name table-name) :type "lisp-expr")
   (get-migration-dir)))

@export
(defun migration-history-p (table-name)
  "T if the table has a migration history, NIL otherwise"
  (probe-file (migration-history-pathname table-name)))

(defun read-migration-history (table-name)
  (read-from-string
   (crane.utils:slurp-file
    (migration-history-pathname table-name))))

@export
(defun insert-migration (table-name digest)
  "Insert a new diff to the migration history"
  (with-open-file (stream (migration-history-pathname table-name)
                          :direction :output
                          :if-does-not-exist :error)
    (format stream "~A" digest)))

@export
(defun rename-migration-history (table-name new-name)
  (rename-file (migration-history-pathname table-name) new-name))
