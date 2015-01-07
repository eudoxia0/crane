(defpackage crane.fixture
  (:use :cl)
  (:documentation "Customizes clos-fixtures for use in Crane."))
(in-package :crane.fixture)

(defmethod clos-fixtures:register-fixture ((instance crane.table:<table>))
  (crane.interface:create% instance))
