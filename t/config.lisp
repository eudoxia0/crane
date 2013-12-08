(in-package :crane-test)

(setf (config-env-var) "CRANE_TEST_ENV")

(defconfig |common|
  `(:application-root
    (merge-pathnames #p"t/"
                     ,(asdf:component-pathname (asdf:find-system :crane-test)))))

(defconfig |dev|
  (list
   :debug t
   :crane `(:migrations-directory
              ,(merge-pathnames
                #p"t/migrations/"
                (asdf:component-pathname (asdf:find-system :crane-test)))
            :databases
              (:name "main"
               :type :sqlite3
               :name ":memory:"))))

(setf (env-var "CRANE_TEST_ENV") "dev")
