(in-package :cl-user)
(defpackage crane-test-asd
  (:use :cl :asdf))
(in-package :crane-test-asd)

(defsystem crane-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:crane
               :fiveam
               :cl-fad)
  :components ((:module "t"
                :serial t
                :components
                ((:file "crane")
                 (:file "config")
                 (:module "utils"
                  :components
                  ((:file "utils")))
                 (:file "meta"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
