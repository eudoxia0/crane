(defsystem crane-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:crane
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "packages")
                 (:file "preliminary")
                 (:module "common"
                  :serial t
                  :components
                  ((:file "util")
                   (:file "connection-spec")
                   (:file "final")))
                 (:module "postgres"
                  :serial t
                  :components
                  ((:file "config")
                   (:file "table")
                   (:file "migration")
                   (:file "queries")
                   (:file "inflate-deflate")
                   (:file "final")))
                 (:module "sqlite3"
                  :serial t
                  :components
                  ((:file "config")
                   (:file "table")
                   (:file "queries")
                   (:file "inflate-deflate")
                   (:file "final"))))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
