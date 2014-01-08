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
            "(:table-options ~A :columns ~A)"
            (serialize-plist (getf digest :table-options))
            (mapcar #'(lambda (plist)
                        (serialize-plist plist))
                    (getf digest :columns))))
  (format stream ")"))

@doc "Insert a new diff to the migration history"
@export
(defun insert-migration (table-name digest)
  (with-open-file (stream (migration-history-pathname table-name)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (if (migration-history-p table-name)
        (progn
          (serialize stream (list digest)))
        (serialize stream (append (read-migration-history table-name)
                                  (list digest))))))

@export
(defun rename-migration-history (table-name new-name)
  (rename-file (migration-history-pathname table-name) new-name))

;;;; Actual table creation and migration

(defparameter +create-table-format-string+
  ;; Are you ready for this one?
  "CREATE TABLE ~A (~{    ~A~#[~:;, ~]~}~A~{    ~A~#[~:;, ~]~});~{~A;~}"
  ;; Is that clear?
  )

@export
(defun create-table (table-name digest)
  (let* ((constraints (crane.sql:create-and-sort-constraints
                      (crane.sql:sqlize table-name)
                      digest))
         (query
           (format nil +create-table-format-string+
                   (crane.sql:sqlize table-name)
                   (getf constraints :definition)
                   (if (getf constraints :internal) "," "")
                   (getf constraints :internal)
                   (getf constraints :external))))
    (crane:execute (crane:prepare query (crane::db table-name)))))

@export
(defun migrate (table-class diff)
  (let* ((table-name (crane.sql:sqlize (crane:table-name table-class)))
         (alterations
          (iter (for column in (getf diff :changes))
            (appending
             (iter (for type in (getf column :diff) by #'cddr)
               (collecting
                (crane.sql:alter-constraint
                  table-name
                  (crane.sql:sqlize (getf column :name))
                  type
                  (cadr (getf (getf column :diff) type))))))))
         (new-columns
           (mapcar #'(lambda (column)
                       (crane.sql:define-column
                           table-name
                           column))
                   (getf diff :additions)))
         (additions
           (iter (for def in new-columns)
             (appending
              (cons (format nil "ALTER TABLE ~A ADD COLUMN ~A"
                            table-name (getf def :definition))
                    (append 
                     (mapcar #'(lambda (internal-constraint)
                                 (crane.sql:add-constraint
                                  table-name
                                  (getf def :name)
                                  internal-constraint))
                             (getf def :internal))
                     (mapcar #'(lambda (external-constraint)
                                 (crane.sql:add-constraint
                                  table-name
                                  (getf def :name)
                                  external-constraint))
                             (getf def :external)))))))
         (deletions
           (mapcar #'(lambda (column-name)
                       (crane.sql:drop-column table-name
                                              (crane.sql:sqlize column-name)))
                   (getf diff :deletions))))
    (when (debugp)
      (pprint table-class)
      (pprint (reduce #'(lambda (a b) (concatenate 'string a ";" b))
                      (append alterations additions deletions))))
    (reduce #'(lambda (a b) (concatenate 'string a ";" b))
            (append alterations additions deletions))))
