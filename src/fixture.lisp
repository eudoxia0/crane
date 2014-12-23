(defpackage crane.fixture
  (:use :cl :cl-annot.doc)
  (:documentation "Customizes clos-fixtures for use in Crane."))
(in-package :crane.fixture)

(defmethod clos-fixtures:register-fixture ((instance crane.table:<table>))
  (crane.interface:create% instance))
