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

@doc "Give constraints Crane-specific names"
(defun constraint-name (column-name type)
  (concatenate 'string "crane_" (sqlize name) "_" (sqlize type)))

@doc "Toggle NULL constraint."
(defun set-null (column-name value)
  (unless value
    "NOT NULL"))

@doc "Toggle UNIQUE constraint."
(defun set-unique (column-name value)
  (when value
    "UNIQUE"))

@doc "Toggle PRIMARY KEY constraint."
(defun set-primary (column-name value)
  (when value
    "PRIMARY KEY"))

@doc "Toggle INDEX pseudo-constraint."
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


@export
(defun create-column-constraints (column)
  (iter (for key in '(:nullp :indexp))
    (collecting (make-constraint (sqlize (getf column :name))
                                 key
                                 (getf column key)))))
