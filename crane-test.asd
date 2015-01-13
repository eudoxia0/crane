(defsystem crane-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:crane
               :fiveam
               :local-time)
  :components ((:module "t"
                :serial t
                :components
                ((:file "packages")
                 (:file "util")
                 (:file "connection-spec")
                 (:module "postgres"
                  :serial t
                  :components
                  ((:file "config")
                   (:file "table")
                   (:file "migration")
                   (:file "queries")
                   (:file "inflate-deflate")))
                 (:file "final"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
