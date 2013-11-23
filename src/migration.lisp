(in-package :crane)
(annot:enable-annot-syntax)

(defun get-configuration ()
  (aif (getf (envy:config) :crane)
       it
       (error 'crane.errors:no-configuration-error)))

(defun get-config-value (property)
  (aif (getf (get-configuration) property)
       it
       (error 'configuration-error :text
              "The configuration for the property ~A was not found."
              property)))

(defun get-migration-dir ()
  (ensure-directories-exist (get-config-value :migration-directory)))

(defun migration-history-file (table-name)
  "Return the pathname to the file containing the migration
history for the table `table-name`."
  (merge-pathnames
   (make-pathname :name (symbol-name table-name) :type "lisp-expr")
   (get-migration-dir)))

(defun migration-history-p (table-name)
  "T if the table has a migration history, NIL otherwise"
  (probe-file (migration-history-file table-name)))

(defun read-migration-history (table-name)
  (read-from-string
   (crane.utils:slurp-file
    (migration-history-file table-name))))

(defun insert-migration (table-name)
  "Insert a new diff to the migration history")

(defun rename-migration-history (table-name new-name)
  (rename-file (migration-history-file) new-name))
