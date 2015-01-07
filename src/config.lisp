(in-package :cl-user)
(defpackage crane.config
  (:use :cl :anaphora)
  (:export :*after-config-hook*
           :setup
           :debugp
           :get-configuration
           :get-config-value)
  (:documentation "Functions for reading and writing from and to the global
  configuration."))
(in-package :crane.config)

(defparameter *config* nil
  "This variable holds Crane's global configuration.")

(defparameter *after-config-hook*
  #'(lambda () nil)
  "A function that gets executed after setup is called. Takes no arguments, does
  nothing by default.")

(defun setup (&key migrations-directory databases (debug nil))
  "Set the configuration."
  (setf *config* (list :migrations-directory migrations-directory
                       :databases databases
                       :debug debug))
  (funcall *after-config-hook*))

(defun debugp ()
  "Determine if Crane is in debug mode."
  (getf *config* :debug))

(defun get-configuration ()
  "Return the configuration object, or signal a no-configuration error."
  (aif *config*
       it
       (error 'crane.errors:no-configuration-error)))

(defun get-config-value (key)
  "Get the value of `key` in the configuration."
  (aif (getf (get-configuration) key)
       it
       (error 'crane.errors:configuration-error
              :key key
              :text "This key is not configured."
              key)))
