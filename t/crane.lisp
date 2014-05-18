(in-package :cl-user)
(defpackage crane-test
  (:use :cl :crane :fiveam))
(in-package :crane-test)

;;;; This has to be done before connecting
(let ((command
        (namestring (merge-pathnames
                     #p"t/setup.sh"
                     (asdf:component-pathname (asdf:find-system :crane))))))
  (asdf:run-shell-command command))
