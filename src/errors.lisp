(defpackage :crane.errors
  (:use :cl))
(in-package :crane.errors)
(annot:enable-annot-syntax)

(define-condition crane-error ()
  ((text :initarg :text :reader text))
  (:report
   (lambda (condition stream)
     (format stream "Crane error: ~A" (text condition)))))

@export
(define-condition configuration-error (crane-error)
  ((key :initarg :key :reader key
        :documentation "The configuration key afflicted by the error."))
  (:report
   (lambda (condition stream)
     (format stream "Configuration error (~A): ~A"
             (key condition)
             (text condition)))))
  
@export
(define-condition no-configuration-error (crane-error) ())

@export
(define-condition empty-table (crane-error) ())

@export
(define-condition query-error (crane-error) ())
