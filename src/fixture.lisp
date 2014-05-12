(defpackage :crane.fixture
  (:use :cl :cl-annot.doc))
(in-package :crane.fixture)

(defmethod clos-fixtures:register-fixture ((instance crane.table:<table>))
  (crane.interface:create% instance))
