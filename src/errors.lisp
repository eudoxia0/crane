(in-package :cl-user)
(defpackage crane.errors
  (:use :cl)
  (:export :configuration-error
           :no-configuration-error
           :empty-table
           :query-error)
  (:documentation "Definition of Crane errors."))
(in-package :crane.errors)

(define-condition crane-error ()
  ((text :initarg :text :reader text))
  (:report
   (lambda (condition stream)
     (format stream "Crane error: ~A" (text condition)))))

(define-condition configuration-error (crane-error)
  ((key :initarg :key :reader key
        :documentation "The configuration key afflicted by the error."))
  (:report
   (lambda (condition stream)
     (format stream "Configuration error (~A): ~A"
             (key condition)
             (text condition))))
  (:documentation "An error in the configuration."))

(define-condition no-configuration-error (crane-error)
  ()
  (:documentation "Crane was not configured."))

(define-condition empty-table (crane-error)
  ()
  (:documentation "Table has no slots."))

(define-condition query-error (crane-error)
  ()
  (:documentation "Error in a query."))
