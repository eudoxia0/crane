(in-package :crane-test)

(defun run-tests ()
  (let ((crane.config::*database-registry* (list)))
    (run! 'crane-test.database:database-tests)
    (run! 'crane-test.config:config-tests)
    (run! 'crane-test.table:table-tests)))
