(in-package :cl-user)
(defpackage crane-test
  (:use :cl :crane :fiveam))

(in-package :cl-user)
(defpackage crane-test.postgres
  (:use :cl :crane :fiveam))

(in-package :cl-user)
(defpackage crane-test.mysql
  (:use :cl :crane :fiveam))

(in-package :cl-user)
(defpackage crane-test.sqlite3
  (:use :cl :crane :fiveam))

(in-package :crane-test)

(def-suite preliminary
  :description "Start with a clean slate")
(in-suite preliminary)

(test configure-migrations-dir
  (finishes
   (crane:setup
    :migrations-directory
    (asdf:system-relative-pathname :crane-test #p"t/migrations/"))))

(test delete-migrations
  (finishes
    (delete-migrations t)))

(run! 'preliminary)
