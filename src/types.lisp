(in-package :cl-user)
(defpackage :crane.types
  (:import-from :cl
                :deftype)
  (:export :integer
           :bigint
           :smallint
           :numeric
           :real
           :character
           :text
           :varchar
           :timestamp
           :datetime))
(in-package :crane.types)

;; Numeric types

(deftype integer () `integer)
(deftype bigint () `integer)
(deftype smallint () `integer)
(deftype numeric () `ratio)
(deftype real () `double-float)

;; Text

(deftype character () `character)
(deftype text () `string)
(deftype varchar () `string)

;; Extra

(deftype timestamp () `string)
(deftype datetime () `string)
