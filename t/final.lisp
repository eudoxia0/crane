(in-package :crane-test.final)

(defmacro run-tests-in-context (major-suite)
  `(progn
     (run! ',major-suite)
     ,@(loop for suite being the external-symbols of
             (find-package :crane-test.generic) collecting
             `(run! ',suite))))

(run! 'crane-test.spec:postgres)
(run! 'crane-test.spec:mysql)
(run! 'crane-test.spec:sqlite3)

(run! 'crane-test.util:tests)

;(run! 'crane-test:setup)

;(run-tests-in-context 'crane-test.postgres:config)
