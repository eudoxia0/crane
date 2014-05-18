(in-package :cl-user)
(defpackage crane-test
  (:use :cl :crane :fiveam))
(in-package :crane-test)

(def-suite preliminary
  :description "Start with a clean slate")
(in-suite preliminary)

(test recreate-databases
  (finishes
    (let ((command
            (namestring (merge-pathnames
                         #p"t/setup.sh"
                         (asdf:component-pathname (asdf:find-system :crane))))))
      (format t "Running ~A" command)
      (asdf:run-shell-command command))))

(run! 'preliminary)
