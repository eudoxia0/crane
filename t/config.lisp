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
                :pass "crane_test_user")))))

(defconfig |dev|
  `(:debug t
    ,@|common|))

(setf (env-var "CRANE_TEST_ENV") "dev")
