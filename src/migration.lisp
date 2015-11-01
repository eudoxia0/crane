(in-package :cl-user)
(defpackage crane.migration
  (:use :cl)
  (:import-from :crane.types
                :sql-type
                :varchar
                :varchar-length)
  (:import-from :crane.meta
                :column
  (:documentation "Implements migrations."))
(in-package :crane.migration)

;;; Migrations

(defclass migration ()
  ()
  (:documentation "A migration is a set of changes required to transform a table to
  another."))
