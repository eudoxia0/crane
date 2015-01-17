(in-package :cl-user)
(defpackage crane.errors
  (:use :cl :trivial-types)
  (:export :crane-error
           :configuration-error
           :no-configuration-error
           :connection-error
           :unsupported-property
           :missing-properties
           :table-definition-error
           :empty-table
           :no-slot-type
           :query-error
           :more-than-one-result
           :key
           :message
           :database-name
           :property-name
           :properties
           :table-name
           :slot-name)
  (:documentation "Definition of Crane errors."))
(in-package :crane.errors)

(define-condition crane-error ()
  ()
  (:documentation "The base class of all Crane errors."))

;;; Configuration errors

(define-condition configuration-error (crane-error)
  ((key :reader key
        :initarg :key
        :type keyword
        :documentation "The configuration key afflicted by the error.")
   (message :reader message
            :initarg :message
            :type string
            :documentation "The message to show along with the error."))
  (:report
   (lambda (condition stream)
     (format stream "Configuration error (~A): ~A"
             (key condition)
             (message condition))))
  (:documentation "An error in the configuration."))

(define-condition no-configuration-error (crane-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Crane was not configured.")))
  (:documentation "Crane was not configured."))

;;; Connection error

(define-condition connection-error (crane-error)
  ((database-name :reader database-name
                  :initarg :database-name
                  :type keyword
                  :documentation "The name of the database with the error."))
  (:documentation "An error when trying to connect to a database."))

(define-condition unsupported-property (connection-error)
  ((property-name :reader property-name
                  :initarg :property-name
                  :type :keyword
                  :documentation "The name of the property with the error."))
  (:report
   (lambda (condition stream)
     (format stream
             "The property '~A' is not supported by the connection parameters of database '~A'."
             (property-name condition)
             (database-name condition))))
  (:documentation "An error when the user passes an unsupported parameter to a
  database connection spec."))

(define-condition missing-properties (connection-error)
  ((properties :reader properties
               :initarg :properties
               :type (proper-list keyword)
               :documentation "A list of properties that were not passed as parameters."))
  (:report
   (lambda (condition stream)
     (format stream
             "The following properties of the connection spec for the database
'~A' have not been provided: ~A"
             (database-name condition)
             (properties condition))))
  (:documentation "An error when a required property is not passed to a database
  connection spec."))

;;; Table errors

(define-condition table-definition-error (crane-error)
  ((table-name :reader table-name
               :initarg :table-name
               :type symbol
               :documentation "The name of the table with the error."))
  (:documentation "An error during table definition."))

(define-condition empty-table (table-definition-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "The table ~A has no slots."
             (table-name condition))))
  (:documentation "Table has no slots."))

(define-condition no-slot-type (table-definition-error)
  ((slot-name :reader slot-name
              :initarg :slot-name
              :type symbol
              :documentation "The name of the slot without type information."))
  (:report
   (lambda (condition stream)
     (format stream "The slot ~A of table ~A has no type."
             (slot-name condition)
             (table-name condition))))
  (:documentation "When a slot has no type."))

;;; Query errors

(define-condition query-error (crane-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Error in query.")))
  (:documentation "Error in a query."))

(define-condition more-than-one-result (query-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Call to single! returned more than one result."))))
