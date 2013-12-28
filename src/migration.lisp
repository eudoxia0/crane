;;;; The first part of this file contains various simple utilities for
;;;; manipulating the migration history of a table. The second part contains
;;;; code that actually creates tables and migrates them. The actual generation
;;;; of table-creating SQL is handled by src/sql.lisp

(defpackage :crane.migration
  (:use :cl :anaphora :crane.utils :cl-annot.doc :iter))
(in-package :crane.migration)
(annot:enable-annot-syntax)

(defun get-migration-dir ()
  (ensure-directories-exist (get-config-value :migrations-directory)))

@doc "Return the pathname to the file containing the migration
history for the table `table-name`."
(defun migration-history-pathname (table-name)
  (merge-pathnames
   (make-pathname :name (symbol-name table-name) :type "lisp-expr")
   (get-migration-dir)))

@doc "T if the table has a migration history, NIL otherwise"
@export
(defun migration-history-p (table-name)
  (probe-file (migration-history-pathname table-name)))

(defun read-migration-history (table-name)
  (read-from-string
   (crane.utils:slurp-file
    (migration-history-pathname table-name))))

@export
(defun get-last-migration (table-name)
  (first (last (read-migration-history table-name))))

(defun serialize-plist (plist)
  (format nil "(~{:~A ~A~#[~:; ~]~})" plist))

@doc "Serialize a list of digests."
(defun serialize (stream list)
  (format stream "(")
  (dolist (digest list)
    (format stream
            "(~A ~A)"
            :class-opts-placeholder ;; Might not actually use this
            (mapcar #'(lambda (plist)
                        (serialize-plist plist))
                    (cadr digest))))
  (format stream ")"))

@doc "Insert a new diff to the migration history"
@export
(defun insert-migration (table-name digest)
  (with-open-file (stream (migration-history-pathname table-name)
                          :direction :output
                          :if-does-not-exist :create)
    (if (migration-history-p table-name)
        (progn
          (serialize stream (list digest)))
        (serialize stream (append (read-migration-history table-name)
                                  (list digest))))))

@export
(defun rename-migration-history (table-name new-name)
  (rename-file (migration-history-pathname table-name) new-name))

@export
(defun migrate (table-class digest)
  (format t "Migrating!~&"))

@export
(defun create-table (table-name digest)
  (let* ((columns
          (iter (for column in (getf digest :columns))
            (collecting (append (list (getf column :name)
                                      (crane.sql:sqlize-type (getf column :type)))
                                (crane.sql:create-column-constraints column)))))
         ;; Each item in COLUMNS follows the format
         ;; (<column name> <column type> <constraint>*...)
         ;; If a constraint is a string, then it goes right into the CREATE
         ;; TABLE statement. If it's a list beginning with the symbol :external,
         ;; it goes into a separate command.
         )

    (print columns)))
