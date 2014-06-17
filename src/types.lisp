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
           :varchar
           :timestamp
           :datetime))
(in-package :crane.types)

;; Numeric types

(deftype int () `integer)
(deftype bigint () `integer)
(deftype smallint () `integer)
(deftype numeric () `ratio)
(deftype double () `double-float)

;; Text

(deftype text () `string)
(deftype varchar () `string)

;; Extra

(deftype timestamp () `string)
(deftype datetime () `string)
