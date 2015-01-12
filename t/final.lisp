(in-package :crane-test.final)

(defmacro run-tests-in-context (major-suite)
  (let ((test-suites (list :table-slots
                           :column-slots
                           :foreign
                           :dereferencing
                           :migrations
                           :queries)))
    `(progn
       (run! ',major-suite)
       ,@(loop for suite in test-suites
               collecting
               `(run! ',(intern (symbol-name suite)
                                (find-package :crane-test.generic)))))))

(run! 'crane-test.spec:postgres)
(run! 'crane-test.spec:mysql)
(run! 'crane-test.spec:sqlite3)

(run! 'crane-test.util:util)

(run! 'crane-test:setup)

(run-tests-in-context 'crane-test.postgres:config)
(run-tests-in-context 'crane-test.postgres:sqlite3)
