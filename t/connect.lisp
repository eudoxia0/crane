(in-package :crane-test)

(defmacro validate (db-type spec)
  `(crane::validate-connection-spec
    :test-db ,db-type ,spec))

(defsuite postgres
  :description "SQLite3 connection spec tests.")
(in-suite postgres)

(test bad-specs
  (signals 'crane.errors:configuration-error
    (validate :postgres '())))

(defsuite sqlite3
  :description "SQLite3 connection spec tests.")
(in-suite sqlite3)

(test bad-specs
  (signals 'crane.errors:configuration-error
    (validate :sqlite3 '())))

(defsuite mysql
  :description "SQLite3 connection spec tests.")
(in-suite mysql)

(test bad-specs
  (signals 'crane.errors:configuration-error
    (validate :mysql '())))


