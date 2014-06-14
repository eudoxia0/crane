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
    (merge-pathnames
     #p"t/migrations/"
     (asdf:system-source-directory :crane-test)))))

(test delete-migrations
  (finishes
    (delete-migrations t)))

(run! 'preliminary)
