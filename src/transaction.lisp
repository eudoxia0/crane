(in-package :cl-user)
(defpackage :crane.transaction
  (:use :cl :anaphora)
  (:import-from :crane.connect
                :*default-db*)
  (:export :with-transaction
           :begin-transaction
           :commit
           :rollback)
  (:documentation "Implements transactions."))
(in-package :crane.transaction)

(defmacro with-transaction ((&optional (db crane.connect:*default-db*))
                            &rest body)
  `(cl-dbi:with-transaction (crane.connect:get-connection ,db)
     ,@body))

(defun begin-transaction (&optional (db *default-db*))
  (dbi:begin-transaction (crane.connect:get-connection db)))

(defun commit (&optional (db *default-db*))
  (dbi:commit (crane.connect:get-connection db)))

(defun rollback (&optional (db *default-db*))
  (dbi:rollback (crane.connect:get-connection db)))
