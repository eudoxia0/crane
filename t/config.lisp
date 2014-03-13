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
               (:type :postgres
                :name "crane_test_db"
                :user "crane_test_user"
                :pass "crane_test_user")
               :interface
               (:type :sqlite3
                :name ":memory:")))))

(defconfig |dev|
  `(:debug t
    ,@|common|))

(setf (osicat:environment-variable "CRANE_TEST_ENV") "dev")
