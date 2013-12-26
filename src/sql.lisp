3;;;; Normally I would use SxQL for table creation and alteration, but the
;;;; sources are too obscure for me to grok, and I don't want to have to
;;;; contribue a pull request just to get basic functionality working. So, for
;;;; now, CREATE TABLE and MIGRATE TABLE statements will be produced as raw
;;;; strings. How horrifying.

(defpackage :crane.sql
  (:use :cl :anaphora :crane.utils :cl-annot.doc :iter))
(in-package :crane.sql)
(annot:enable-annot-syntax)

@export
(defun sqlize (obj)
  (typecase obj
    (symbol
     (sqlize (symbol-name obj)))
    (string
     (map 'string #'(lambda (char) (if (eql char #\-) #\_ char))
          obj))))

(defun constraint-name (column-name type)
  (concatenate 'string "crane_" (sqlize name) "_" (sqlize type)))

(defun set-null (column-name value)
  (unless value
    (format nil "NOT NULL")))

(defun set-index (table-name column-name value)
  (if value
    (list :external
          (format nil "CREATE INDEX ~A ON ~A (~A)"
                  (constraint-name column-name 'index)
                  table-name
                  value))
    (list :external
        (format nil "DROP INDEX ~A ON ~A"
                (constraint-name column-name 'index)
                table-name))))

(defun create-constraint (cons-type value)
  (case cons-type
    (nullp
     (if value "NULL" "NOT NULL"))))

(defun make-constraint (column-name cons-type value)
  (format nil "CONSTRAINT ~A_~A ~A" column-name cons-type
          (create-constraint cons-type value)))

@export
(defun create-column-constraints (column)
  (iter (for key in '(:nullp :indexp))
    (collecting (make-constraint (sqlize (getf column :name))
                                 key
                                 (getf column key)))))
