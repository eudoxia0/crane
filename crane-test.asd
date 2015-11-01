(defsystem crane-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:crane
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "crane")
                 (:file "database")
                 (:file "config")
                 (:file "table")
                 (:file "final"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
