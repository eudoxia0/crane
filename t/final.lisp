(in-package :crane-test.final)

(def-suite preliminary
  :description "Start with a clean slate")
(in-suite preliminary)

(test delete-migrations
  (finishes
    (let ((dir (asdf:system-relative-pathname :crane-test #p"t/migrations/")))
      (when (fad:directory-exists-p dir)
        (fad:delete-directory-and-files dir)))))

(let ((tests '(crane-test.util:util-tests

               crane-test.spec:postgres
               crane-test.spec:mysql
               crane-test.spec:sqlite3

               preliminary
               crane-test.postgres:config
               crane-test.postgres:table-slots
               crane-test.postgres:column-slots
               crane-test.postgres:foreign
               crane-test.postgres:migrations
               crane-test.postgres:queries
               crane-test.postgres:inflate-deflate)))
  (loop for test in tests do
    (run! test)))
