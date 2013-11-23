(in-package :crane)
(annot:enable-annot-syntax)

(defun get-configuration ()
  (aif (getf (envy:config) :crane)
       it
       (error 'crane.errors:no-configuration-error)))

(defun get-config-value (property)
  (aif (getf (get-configuration) property)
       it
       (error 'configuration-error :text
              "The configuration for the property ~A was not found."
              property)))
