(in-package :cl-user)
(defpackage :crane.types
  (:import-from :cl
                :deftype)
  (:export :int
           :bigint
           :smallint
           :numeric
           :double
           :text
           :string
           :varchar
           :timestamp
           :datetime)
  (:documentation "Implements the database types."))
(in-package :crane.types)

;; Numeric types

(deftype int () `cl:integer)
(deftype bigint () `cl:integer)
(deftype smallint () `cl:integer)
(deftype numeric () `cl:ratio)
(deftype double () `cl:double-float)

;; Text

(deftype text () `cl:string)
(deftype string () `cl:string)
(deftype varchar () `cl:string)

;; Extra

(deftype timestamp () `cl:string)
(deftype datetime () `cl:string)
