(in-package :cl-user)
(defpackage :crane.errors
  (:use :cl :annot.doc)
  (:export :configuration-error
           :no-configuration-error
           :empty-table
           :query-error))
(in-package :crane.errors)
(annot:enable-annot-syntax)

(define-condition crane-error ()
  ((text :initarg :text :reader text))
  (:report
   (lambda (condition stream)
     (format stream "Crane error: ~A" (text condition)))))

@doc "An error in the configuration."
(define-condition configuration-error (crane-error)
  ((key :initarg :key :reader key
        :documentation "The configuration key afflicted by the error."))
  (:report
   (lambda (condition stream)
     (format stream "Configuration error (~A): ~A"
             (key condition)
             (text condition)))))

@doc "Crane was not configured."
(define-condition no-configuration-error (crane-error) ())

@doc "Table has no slots."
(define-condition empty-table (crane-error) ())

@doc "Error in a query."
(define-condition query-error (crane-error) ())
