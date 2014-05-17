;;;; Read and write the configuration

(in-package :cl-user)
(defpackage :crane.config
  (:use :cl :annot.doc :anaphora))
(in-package :crane.config)
(annot:enable-annot-syntax)

@doc "This variable holds Crane's global configuration."
(defparameter *config* nil)

@doc "Set the configuration."
@export
(defun setup (config)
  (setf *config* config))

@doc "Determine if Crane is in debug mode."
@export
(defun debugp ()
  (getf *config* :debug))

@doc "Return the configuration object, or signal a no-configuration error."
@export
(defun get-configuration ()
  (aif *config*
       it
       (error 'crane.errors:no-configuration-error)))

@doc "Get the value of `key` in the configuration."
@export
(defun get-config-value (key)
  (aif (getf (get-configuration) key)
       it
       (error 'crane.errors:configuration-error
              :key key
              :text "This key is not configured."
              key)))
