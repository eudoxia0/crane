(defpackage crane.migration
  (:use :cl :anaphora :iter)
  (:import-from :crane.config
                :get-config-value
                :debugp)
  (:import-from :crane.meta
                :table-name
                :digest
                :diff-digest)
  (:export :migration-history-p
           :get-last-migration
           :insert-migration
           :rename-migration-history
           :create-table
           :migrate
           :build
           :delete-migrations)
  (:documentation "The first part of this package contains various simple
  utilities for manipulating the migration history of a table. The second part
  contains code that actually creates tables and migrates them. The actual
  generation of table-creating SQL is handled by crane.sql."))
(in-package :crane.migration)

(defun get-migration-dir ()
  (ensure-directories-exist
   (cl-fad:pathname-as-directory (get-config-value :migrations-directory))))

(defun migration-history-pathname (table-name)
  "Return the pathname to the file containing the migration history for the
table `table-name`."
  (merge-pathnames
   (make-pathname :name (symbol-name table-name) :type "lisp-expr")
   (get-migration-dir)))

(defun migration-history-p (table-name)
  "T if the table has a migration history, NIL otherwise."
  (probe-file (migration-history-pathname table-name)))

(defun read-migration-history (table-name)
  (read-from-string
   (uiop:read-file-string
    (migration-history-pathname table-name))))

(defun get-last-migration (table-name)
  (first (last (read-migration-history table-name))))

(defun serialize-plist (plist)
  (format nil "(~{:~A ~A~#[~:; ~]~})" plist))

(defun serialize (stream list)
  "Serialize a list of digests."
  (format stream "(")
  (dolist (digest list)
    (format stream
            "(:table-options ~A :columns ~A)"
            (serialize-plist (getf digest :table-options))
            (mapcar #'(lambda (plist)
                        (serialize-plist plist))
                    (getf digest :columns))))
  (format stream ")"))

(defun insert-migration (table-name digest)
  "Insert a new diff to the migration history"
  (with-open-file (stream (migration-history-pathname table-name)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (if (migration-history-p table-name)
        (progn
          (serialize stream (list digest)))
        (serialize stream (append (read-migration-history table-name)
                                  (list digest))))))

(defun rename-migration-history (table-name new-name)
  (rename-file (migration-history-pathname table-name) new-name))

;;;; Actual table creation and migration

(defparameter +create-table-format-string+
  ;; Are you ready for this one?
  "CREATE TABLE ~A (~{    ~A~#[~:;, ~]~}~A~{    ~A~#[~:;, ~]~});~{~A;~}"
  ;; Is that clear?
  )

(defun create-table (table-name digest)
  (let* ((constraints (crane.sql:create-and-sort-constraints
                       table-name
                       digest
                       (crane.meta:table-database (find-class table-name))))
         (query
           (format nil +create-table-format-string+
                   (crane.sql:sqlize (table-name (find-class table-name)))
                   (getf constraints :definition)
                   (if (getf constraints :internal) "," "")
                   (getf constraints :internal)
                   (getf constraints :external)))
         (conn (crane.connect:get-connection (crane.meta:table-database
                                              (find-class table-name)))))
    (format t "~&Query: ~A~&" query)
    (dbi:execute (dbi:prepare conn query))))

(defun migrate (table-class diff)
  (let* ((table-name (crane.meta:table-name table-class))
         (alterations
          (iter (for column in (getf diff :changes))
            (appending
             (iter (for type in (getf column :diff) by #'cddr)
               (collecting
                (crane.sql:alter-constraint
                  table-name
                  (getf column :name)
                  type
                  (cadr (getf (getf column :diff) type))))))))
         (new-columns
           (mapcar #'(lambda (column)
                       (crane.sql:define-column
                           table-name
                           column
                         (crane.meta:table-database
                          (find-class table-name))))
                   (getf diff :additions)))
         (additions
           (iter (for def in new-columns)
             (appending
              (cons (format nil "ALTER TABLE ~A ADD COLUMN ~A"
                            (crane.sql:sqlize table-name) (getf def :definition))
                    (append
                     (mapcar #'(lambda (internal-constraint)
                                 (crane.sql:add-constraint
                                  table-name
                                  internal-constraint))
                             (getf def :internal))
                     (getf def :external))))))
         (deletions
           (mapcar #'(lambda (column-name)
                       (crane.sql:drop-column table-name
                                              column-name))
                   (getf diff :deletions)))
	 (conn (crane.connect:get-connection (crane.meta:table-database
                                              (find-class table-name)))))
    (dolist (query (append alterations additions deletions))
      (format t "~&Query: ~A~&" query)
      (dbi:execute (dbi:prepare conn query)))))

(defun build (table-name)
  (unless (crane.meta:abstractp (find-class table-name))
    (if (migration-history-p table-name)
        (let ((diff (diff-digest
                     (get-last-migration table-name)
                     (digest (find-class table-name)))))
          (if (or (getf diff :additions)
                  (getf diff :deletions)
                  (getf diff :changes))
              (progn
                (when (debugp)
                  (format t "~&Diff for '~A': ~A~&" table-name diff))
                (migrate (find-class table-name) diff)
                (insert-migration table-name
                                  (digest (find-class table-name))))))
        (let ((digest (digest (find-class table-name))))
          (insert-migration table-name digest)
          (create-table table-name digest)))))

(defun delete-migrations (&optional force)
  (when (or force (yes-or-no-p "Really delete migrations?"))
    (fad:delete-directory-and-files
     (get-migration-dir))))
