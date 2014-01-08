(in-package :cl-user)
(defpackage crane
  (:use :cl :anaphora :iter :cl-annot.doc :sxql)
  (:export :table-class
           :table-name
           :abstract-p
           :<table>))
(in-package :crane)
