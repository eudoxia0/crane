(in-package :crane-test.postgres)

(defmacro validate (db-type spec)
  `(crane.connect::validate-connection-spec
    :test-db ,db-type ,spec))

(defmacro generate-bad-tests (db-type)
  (let ((specs
          '(()
            (:name "foo")
            (:user "john" :pass "1234")
            (:name "foo" :user "john")
            (:name "foo" :some-other-key t)
            (:some-other-key t))))
    `(progn
       ,@(loop for spec in specs
               for i from 0 to (1- (length specs))
               collecting
               `(test ,(intern (concatenate 'string
                                            "bad-spec-"
                                            (write-to-string i))
                               *package*)
                  (signals crane.errors:configuration-error
                    (validate ,db-type ',spec)))))))

(defmacro generate-good-tests (db-type)
  (let ((specs
          '((:name "foo" :user "john" :pass "1234"))))
    `(progn
       ,@(loop for spec in specs
               for i from 0 to (1- (length specs))
               collecting
               `(test ,(intern (concatenate 'string
                                            "good-spec-"
                                            (write-to-string i))
                               *package*)
                  (finishes
                    (validate ,db-type ',spec)))))))

(def-suite postgres
  :description "Postgres connection spec tests.")
(in-suite postgres)

(generate-bad-tests :postgres)
(generate-good-tests :postgres)


(def-suite mysql
  :description "MySQL connection spec tests.")
(in-suite mysql)


(generate-bad-tests :mysql)
(generate-good-tests :mysql)


(def-suite sqlite3
  :description "SQLite3 connection spec tests.")
(in-suite sqlite3)

(test bad-specs
  (signals 'crane.errors:configuration-error
    (validate :sqlite3 '())
    (validate :sqlite3 '(:some-other-key t))))

(test bad-specs
  (finishes
    (validate :sqlite3 '(:name "my-db"))))


(def-suite connecting
    :description "Test that we can actually connect.")
(in-suite connecting)

(test connect
  (finishes
    (crane.connect:connect)))

(test main-db
  (is (equal :main crane.connect:*default-db*)))


(run! 'postgres)
(run! 'mysql)
(run! 'sqlite3)
(run! 'connecting)
