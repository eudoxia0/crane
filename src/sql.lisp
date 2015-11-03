;;;; Normally I would use SxQL for table creation and alteration, but the
;;;; sources are too obscure for me to grok, and I don't want to have to
;;;; contribue a pull request just to get basic functionality working. So, for
;;;; now, CREATE TABLE and MIGRATE TABLE statements will be produced as raw
;;;; strings. How horrifying.

(in-package :cl-user)
(defpackage crane.sql
  (:use :cl :anaphora :crane.util :iter)
  (:import-from :sxql
                :*quote-character*)
  (:export :sqlize
           :make-constraint
           :define-column
           :create-and-sort-constraints
           :add-constraint
           :drop-constraint
           :alter-constraint
           :drop-column)
  (:documentation "This module handles the generation of SQL for table definition and migration."))
(in-package :crane.sql)

(defun sqlize (obj)
  "Turn a symbol or a string into its SQL representation. Identical to the
behaviour of SxQL."
  (typecase obj
    (symbol
     (sqlize (symbol-name obj)))
    (string
     (format nil "~A~A~A"
             (or *quote-character* "")
             (string-downcase obj)
             (or *quote-character* "")))))

(defun constraint-name (table-name column-name type)
  "Give constraints Crane-specific names"
  (sqlize (concatenate 'string "crane_" (symbol-name table-name) "_"
                       (symbol-name column-name) "_" (symbol-name type))))

(defun set-null (column-name value)
  "Toggle NULL constraint."
  (unless value
    (concatenate 'string "CHECK (" (sqlize column-name) " IS NOT NULL)")))

(defun set-unique (column-name value)
  "Toggle UNIQUE constraint."
  (when value
    (concatenate 'string "UNIQUE (" (sqlize column-name) ")")))

(defun set-primary (column-name value)
  "Toggle PRIMARY KEY constraint."
  (when value
    (concatenate 'string "PRIMARY KEY (" (sqlize column-name) ")")))

(defun set-index (table-name column-name value)
  "Toggle INDEX pseudo-constraint."
  (if value
    (list :external
          (format nil "CREATE INDEX ~A ON ~A (~A)"
                  (constraint-name table-name column-name :index)
                  (sqlize table-name)
                  (sqlize column-name)))
    (list :external
          (format nil "DROP INDEX ~A ON ~A"
                  (constraint-name table-name column-name :index)
                  (sqlize table-name)))))

(defparameter +referential-actions+
  (list :cascade "CASCADE"
        :restrict "RESTRICT"
        :no-action "NO ACTION"
        :set-null "SET NULL"
        :set-default "SET DEFAULT"))

(defun map-ref-action (action)
  (aif (getf +referential-actions+ action)
       it
       (error "No such referential action: ~A" action)))

(defun foreign (local foreign-table &key (on-delete :no-action) (on-update :no-action))
  (format nil "FOREIGN KEY (~A) REFERENCES ~A(id) ON DELETE ~A ON UPDATE ~A"
          (sqlize local)
          (sqlize foreign-table)
          (map-ref-action on-delete)
          (map-ref-action on-update)))

(defun make-constraint (table-name column-name type value)
  "A constraint from its type and values, if it can be created (eg :nullp t
doesn't create a constraint, but :nullp nil creates a NOT NULL constraint)."
  (if (eql type :indexp)
      ;; :indexp is treated especially, because it generates an external command
      ;; which already includes the constraint name
      (when value (set-index table-name column-name value))
      (aif (ecase type
             (:nullp
              (set-null column-name value))
             (:uniquep
              (set-unique column-name value))
             (:primaryp
              (set-primary column-name value))
             (:foreign
              (when value (apply #'foreign (cons column-name (if (listp value) value (list value)))))))
           (concatenate 'string
                        "CONSTRAINT "
                        (constraint-name table-name column-name type)
                        " "
                        it))))

(defun create-column-constraints (table-name column)
  (let ((column-name (getf column :name)))
    (remove-if #'null
               (iter (for key in '(:nullp :uniquep :primaryp :indexp :foreign))
                 (collecting (make-constraint table-name
                                              column-name
                                              key
                                              (getf column key)))))))

(defun autoincrement-sql (database-type)
  (case database-type
    (:postgres "SERIAL")
    (:mysql    "AUTO_INCREMENT")
    (:sqlite3  "PRIMARY KEY AUTOINCREMENT")))

(defun define-column (table-name column database-name)
  "A column definition from the digest of its slot, name and name of the
database it's table belongs to"
  (let* ((column-definition
           (let ((column-type (symbol-name (getf column :type))))
             (format nil "~A ~A"
                     (sqlize (getf column :name))
                     (if (equal (symbol-name (getf column :name)) "ID")
                         ;; Autoincrement: Postgres requieres the 'SERIAL' keyword
                         ;; only, without the database type, so we have to choose
                         ;; here whether to display the include the type in the
                         ;; CREATE TABLE statement on that basis
                         (let* ((db-type (crane.connect:database-type
                                          (crane.connect:get-db
                                           database-name)))
                                (sql (autoincrement-sql db-type)))
                           (if (eq db-type :postgres)
                               sql
                               (format nil "~A ~A" column-type sql)))
                         column-type))))
         (constraints
           (create-column-constraints table-name column))
         (internal-constraints
           (remove-if-not #'stringp constraints))
         (external-constraints
           (mapcar #'cadr
                   (remove-if-not #'listp constraints))))
    (list :definition column-definition
          :internal internal-constraints
          :external external-constraints)))

(defun create-and-sort-constraints (table-name digest database-name)
  "A plist of different types of constraints from a table digest."
  (let ((definitions
          (mapcar #'(lambda (col)
                      (define-column table-name col database-name))
                  (getf digest :columns))))
    (list :definition (mapcar #'(lambda (def) (getf def :definition)) definitions)
          :internal (reduce #'append
                            (mapcar #'(lambda (def) (getf def :internal)) definitions))
          :external (reduce #'append
                            (mapcar #'(lambda (def) (getf def :external)) definitions)))))

;;;; Constraint processing is stupid, I wish I was coding something more fun :c

;;;; Alter Table

(defun add-constraint (table-name body)
  "SQL to add a constraint to a table."
  (format nil "ALTER TABLE ~A ADD ~A"
          (sqlize table-name)
          body))

(defun drop-constraint (table-name column-name type)
  "SQL to drop a constraint from a table."
  (format nil "ALTER TABLE ~A DROP CONSTRAINT ~A"
          (sqlize table-name)
          (constraint-name table-name column-name type)))

(defun alter-constraint (table-name column-name type value)
  "SQL to alter a constraint in a table."
  (if (member type (list :primaryp :uniquep :indexp :foreign :check))
      (if value
          ;; The constraint wasn't there, add it
          (aif (make-constraint table-name column-name type value)
               (add-constraint table-name it))
          ;; The constraint has been dropped
          (drop-constraint table-name
                           column-name
                           type))
      ;; NULL constraint
      (if (null value)
          ;; Set null
          (aif (make-constraint table-name column-name :nullp nil)
               (add-constraint table-name it))
          ;; Remove null constraint
          (drop-constraint table-name
                           column-name
                           type))))

(defun drop-column (table-name column-name)
  "SQL to drop a column, given the table and column names."
  (sxql:yield (sxql:alter-table table-name
                (sxql:drop-column column-name))))
