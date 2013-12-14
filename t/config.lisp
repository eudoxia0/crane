(in-package :crane-test)

(setf (config-env-var) "CRANE_TEST_ENV")

(defconfig |common|
  `(:application-root
    (merge-pathnames #p"t/"
                     ,(asdf:component-pathname (asdf:find-system :crane-test)))
    :crane (:migrations-directory
              ,(merge-pathnames
                #p"t/migrations/"
                (asdf:component-pathname (asdf:find-system :crane-test)))
            :databases
              (:main
               (:type :sqlite3
                :name ":memory:")))))

(defconfig |dev|
  `(:debug t
    ,@|common|))

(setf (env-var "CRANE_TEST_ENV") "dev")
