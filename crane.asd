(defsystem crane
  :version "0.4"
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :homepage "http://eudoxia.me/crane/"
  :depends-on (:closer-mop
               :sxql
               :dbi
               :clos-fixtures
               :uiop
               :local-time)
  :components ((:module "src"
                :serial t
                :components
                ((:file "serialize")
                 (:file "database")
                 (:file "types")
                 (:file "convert")
                 (:module "backend"
                  :components
                  ((:file "postgres")
                   (:file "mysql")
                   (:file "sqlite3")))
                 (:file "config")
                 (:module "table"
                  :serial t
                  :components
                  ((:file "table")
                   (:file "serialize")
                   (:file "sql")
                   (:file "diff")
                   (:file "create")
                   (:file "alter")))
                 (:file "session"))))
  :description "An ORM for Common Lisp."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (load-op crane-test))))
