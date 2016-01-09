(in-package :cl-user)
(defpackage crane.util
  (:use :cl)
  (:export :symbol-to-sql))
(in-package :crane.util)

(defun symbol-to-sql (symbol)
  "Return a valid SQL string equivalent of a Lisp symbol. Identical to SxQL's
behaviour."
  (let ((sxql:*quote-character* #\"))
    (sxql:yield (sxql.operator:detect-and-convert symbol))))
