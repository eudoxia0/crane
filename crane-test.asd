(defsystem crane-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:crane
               :dbd-postgres
               :dbd-mysql
               :dbd-sqlite3
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "crane")
                 (:file "database")
                 (:file "config")
                 (:module "table"
                  :serial t
                  :components
                  ((:file "table")
                   (:file "serialize")
                   (:file "sql")))
                 (:file "session")
                 (:file "query")
                 (:file "final"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
