(in-package :crane-test)

(defun run-tests ()
  (let ((crane.config::*database-registry* (list)))
    (when (uiop:getenv "TRAVIS")
      ;; Only run database connection tests on Travis, since I don't have
      ;; Postgres and MySQL set up on my machine
      (run! 'crane-test.database:database-tests))
    (run! 'crane-test.config:config-tests)
    (run! 'crane-test.table:table-tests)
    (run! 'crane-test.table.serialize:serialization-tests)
    (run! 'crane-test.table.sql:sql-dsl)
    (run! 'crane-test.session:session-tests)
    (run! 'crane-test.query:query-tests)))
