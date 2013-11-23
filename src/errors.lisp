(defpackage :crane.errors
  (:use :cl))
(in-package :crane.errors)
(annot:enable-annot-syntax)

@export
(define-condition configuration-error (error)
  ((text :initarg :text :reader text)))

@export
(define-condition no-configuration-error (configuration-error) ())
