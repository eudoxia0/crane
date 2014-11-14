;;;; Read and write the configuration

(in-package :cl-user)
(defpackage :crane.config
  (:use :cl :annot.doc :anaphora)
  (:export :setup
           :debugp
           :get-configuration
           :get-config-value))
(in-package :crane.config)
(annot:enable-annot-syntax)

@doc "This variable holds Crane's global configuration."
(defparameter *config* nil)

@doc "Set the configuration."
(defun setup (&key migrations-directory databases (debug nil))
  (setf *config* (list :migrations-directory migrations-directory
                       :databases databases
                       :debug debug)))

@doc "Determine if Crane is in debug mode."
(defun debugp ()
  (getf *config* :debug))

@doc "Return the configuration object, or signal a no-configuration error."
(defun get-configuration ()
  (aif *config*
       it
       (error 'crane.errors:no-configuration-error)))

@doc "Get the value of `key` in the configuration."
(defun get-config-value (key)
  (aif (getf (get-configuration) key)
       it
       (error 'crane.errors:configuration-error
              :key key
              :text "This key is not configured."
              key)))
