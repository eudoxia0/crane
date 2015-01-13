(in-package :cl-user)
(defpackage crane-test
  (:use :cl :crane :fiveam))

(defpackage crane-test.postgres
  (:use :cl :crane :fiveam))

(defpackage crane-test.mysql
  (:use :cl :crane :fiveam))

(defpackage crane-test.sqlite3
  (:use :cl :crane :fiveam))

(in-package :crane-test)

(def-suite preliminary
  :description "Start with a clean slate")
(in-suite preliminary)

(test delete-migrations
  (finishes
    (let ((dir (asdf:system-relative-pathname :crane-test #p"t/migrations/")))
      (when (fad:directory-exists-p dir)
        (fad:delete-directory-and-files dir)))))

(run! 'preliminary)
