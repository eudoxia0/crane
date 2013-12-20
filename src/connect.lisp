(in-package :crane)
(annot:enable-annot-syntax)

(defparameter +system-mapping+
  (list :postgres :dbd-postgres
        :sqlite3  :dbd-sqlite3
        :mysql    :dbd-mysql))

@doc "Load the ASDF system for the specified database module."
(defun load-driver (driver)
  (let ((system (getf +system-mapping+ driver)))
    #+quicklisp (ql:quickload system :verbose nil)
    #-quicklisp (asdf:load-system system :verbose nil)))

(defparameter +db-params+
  '(:postgres ((:name :database-name)
               (:user :username)
               (:pass :password)
               (:host :host "localhost")
               (:port :port 5432)
               (:ssl  :use-ssl :no))
    :sqlite3 ((:name :database-name))
    :mysql ((:name :database-name)
            (:user :username)
            (:pass :password)
            (:host :host "localhost")
            (:port :port 3306))))

(defun validate-connection-spec (db database-type spec)
  (let* ((reference-spec (getf +db-params+ database-type))
         (normalized-spec
           (iter (for line in reference-spec)
                 (appending (list (car line) (cadr line)))))
         (required-keys
           (iter (for line in reference-spec)
                 (if (not (cddr line))
                     (collecting (car line)))))
         (final-spec (list)))
    (iter (for key in spec by #'cddr)
          (aif (getf normalized-spec key)
               (setf (getf final-spec key) (getf spec key))
               (error 'crane.errors:configuration-error
                      :key (list :databases :-> db :-> key)
                      :text (format nil "The property '~A' is not supported by
the connection spec of the database '~A'" key db))))
    (aif (set-difference
          required-keys
          (crane.utils:plist-keys final-spec))
         (error 'crane.errors:configuration-error
                :key (list :databases :-> db)
                :text (format nil "The following properties of the connection
spec for the database '~A' have not been provided: ~A" db it))
         final-spec)))

@doc "A map from database names to connections."
(defparameter *db* (make-hash-table))

(defun connect-spec (db spec)
  (aif (getf +system-mapping+ (getf spec :type))
    (progn
      (load-driver it)
      (apply #'dbi-connect
        (cons (getf spec :type)
              (validate-connection-spec db (getf spec :type) spec))))
    (error 'crane.errors:configuration-error
           :key (list :databases :-> db)
           :text (format nil
                         "The database type '~A' is not supported by DBI yet."
                         (getf spec :type)))))

@doc "Connect to all the databases specified in the configuration."
(defun connect ()
  (aif (crane.utils:get-config-value :databases)
       (iter (for (db spec) on it by #'cddr)
         (if (gethash db *db*)
             (error 'crane.errors:configuration-error
                    :key :databases
                    :text "Two databases have the same name.")
             (setf (gethash db *db*) (connect-spec db spec))))
       (error 'crane.errors:configuration-error
              :key :databases
              :text "No databases found.")))
