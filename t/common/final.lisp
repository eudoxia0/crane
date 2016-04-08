(in-package :crane-test)

(run! 'crane-test.util:util-tests)

(run! 'crane-test.spec:postgres)

(run! 'crane-test.spec:mysql)

(run! 'crane-test.spec:sqlite3)
