(in-package :crane-test)

(defmacro validate (db-type spec)
  `(crane.connect::validate-connection-spec
    :test-db ,db-type ,spec))

(defun process-specs (db-type test-cases)
  (cons 'progn
        (loop for case in test-cases collecting
              `(validate ,db-type (quote ,case)))))

(defmacro shared-bad-specs (db-type)
  "Bad connection specs shared by both PostgreSQL and MySQL."
  (let ((test-cases
          '(()
            (:name "foo")
            (:user "john" :pass "1234")
            (:name "foo" :user "john")
            (:name "foo" :some-other-key t)
            (:some-other-key t))))
    (process-specs db-type test-cases)))

(defmacro shared-good-specs (db-type)
  "Valid connection specs for PostgreSQL and MySQL."
  (let ((test-cases
          '((:name "foo" :user "john" :pass "1234"))))
    (process-specs db-type test-cases)))

(def-suite postgres
  :description "SQLite3 connection spec tests.")
(in-suite postgres)

(test bad-specs
  (signals crane.errors:configuration-error
    (shared-bad-specs :postgres)))

(test good-specs
  (finishes
    (shared-good-specs :postgres)))

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


(def-suite mysql
  :description "SQLite3 connection spec tests.")
(in-suite mysql)

(test bad-specs
  (signals crane.errors:configuration-error
    (shared-bad-specs :mysql)))

(test good-specs
  (finishes
    (shared-good-specs :mysql)))

(def-suite connecting
    :description "Test that we can actually connect.")
(in-suite connecting)

(test connect
  (finishes
    (crane.connect:connect)))

(test main-db
  (is (equal :main crane.connect:*default-db*)))

(run! 'postgres)
(run! 'sqlite3)
(run! 'mysql)
(run! 'connecting)
