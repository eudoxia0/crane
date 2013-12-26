3;;;; Normally I would use SxQL for table creation and alteration, but the
;;;; sources are too obscure for me to grok, and I don't want to have to
;;;; contribue a pull request just to get basic functionality working. So, for
;;;; now, CREATE TABLE and MIGRATE TABLE statements will be produced as raw
;;;; strings. How horrifying.

(defpackage :crane.sql
  (:use :cl :anaphora :crane.utils :cl-annot.doc :iter))
(in-package :crane.sql)
(annot:enable-annot-syntax)

(defun make-constraint (column-name cons-type value)
  (format nil "CONSTRAINT ~A_~A ~A ~A"
          column-name cons-type cons-type value))


@export
(defun process-column-constraints (column)
  (iter (for key in '(:nullp :uniquep :primaryp :indexp))
    (collecting (make-constraint (getf column :name)
                                 key
                                 (getf column key)))))

@export
(defun sqlize (obj)
  (typecase obj
    (symbol
     (sqlize (symbol-name obj)))
    (string
     (map 'string #'(lambda (char) (if (eql char #\-) #\_ char))
          obj))))
