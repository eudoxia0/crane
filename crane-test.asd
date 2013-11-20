(in-package :cl-user)
(defpackage crane-test-asd
  (:use :cl :asdf))
(in-package :crane-test-asd)

(defsystem crane-test
  :author "Fernando Borretti"
  :license "LLGPL"
  :depends-on (:crane :fiveam)
  :components ((:module "t"
                :components
                ((:file "crane")
                 (:file "meta"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
