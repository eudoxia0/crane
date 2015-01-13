(in-package :crane-test.final)

(let ((tests '(crane-test.util:util-tests

               crane-test.spec:postgres
               crane-test.spec:mysql
               crane-test.spec:sqlite3

               crane-test:preliminary
               crane-test.postgres:config
               crane-test.postgres:table-slots
               crane-test.postgres:column-slots
               crane-test.postgres:foreign
               crane-test.postgres:migrations
               crane-test.postgres:queries
               crane-test.postgres:inflate-deflate)))
  (loop for test in tests do
    (run! test)))
