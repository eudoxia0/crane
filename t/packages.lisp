(in-package :cl-user)
(defpackage crane-test
  (:use :cl :crane :fiveam)
  (:export :preliminary))

(defpackage crane-test.util
  (:use :cl :fiveam)
  (:export :util-tests))

(defpackage crane-test.spec
  (:use :cl :fiveam)
  (:export :postgres
           :mysql
           :sqlite3))

(defpackage crane-test.postgres
  (:use :cl :crane :fiveam)
  (:export :config
           :table-slots
           :column-slots
           :foreign
           :migrations
           :queries
           :inflate-deflate))

(defpackage crane-test.mysql
  (:use :cl :crane :fiveam))

(defpackage crane-test.sqlite3
  (:use :cl :crane :fiveam))

(defpackage crane-test.final
  (:use :cl :fiveam))

(in-package :crane-test)

(def-suite preliminary
  :description "Start with a clean slate")
(in-suite preliminary)

(test delete-migrations
  (finishes
    (let ((dir (asdf:system-relative-pathname :crane-test #p"t/migrations/")))
      (when (fad:directory-exists-p dir)
        (fad:delete-directory-and-files dir)))))
