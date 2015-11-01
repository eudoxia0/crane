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
               :local-time
               :yason)
  :components ((:module "src"
                :serial t
                :components
                ((:file "database")
                 (:file "config")
                 (:file "types")
                 (:file "table")
                 (:file "serialize"))))
  :description "An ORM for Common Lisp."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (load-op crane-test))))
