(in-package :cl-user)
(defpackage crane.query
  (:use :cl)
  (:import-from :crane.session
                :*session*)
  (:export :create
           :create-instance
           :save
           :del
           :filter
           :total)
  (:documentation "The crane.query package implements versions of the database
  interface methods in the crane.session package that work on the current
  @c(*session*) object. Basically, it's a simpler interface."))
(in-package :crane.query)

(defun create (class-name &rest arguments)
  "Create an instance of @c(class-name) using the initargs @c(arguments), and
immediately store it in the database."
  (let ((instance (apply #'make-instance (cons class-name arguments))))
    (crane.session:create *session* instance)
    instance))

(defun create-instance (instance)
  "Given an instance of a database object that has not yet been stored, store it
in the database.

Returns the instance."
  (crane.session:create *session* instance))

(defun save (instance)
  "Save any changes made to an instance of a database object."
  (crane.session:save *session* instance))

(defun del (instance)
  "Delete this instance from the database."
  (crane.session:delete-instance *session* instance))

(defun filter (class-name &rest constraints)
  "Return a list of instances that meet the constraints."
  (apply #'crane.session:filter (cons *session* (cons class-name constraints))))

(defun total (class-name &rest constraints)
  "Return the number of instances of @c(class-name) that satisfy the optional
@(constraints)."
  (apply #'crane.session:select (append
                                 (list '(:count :id) *session* class-name)
                                 constraints)))
