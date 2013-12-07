(defpackage :crane.errors
  (:use :cl))
(in-package :crane.errors)
(annot:enable-annot-syntax)

(define-condition crane-error ()
  ((text :initarg :text :reader text)))

@export
(define-condition configuration-error (crane-error) ())
  
@export
(define-condition no-configuration-error (configuration-error) ())

@export
(define-condition empty-table (crane-error) ())
