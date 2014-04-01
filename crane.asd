(in-package :cl-user)
(defpackage crane-asd
  (:use :cl :asdf))
(in-package :crane-asd)

(defsystem crane
  :version "0.2"
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:closer-mop
               :anaphora
               :sxql
               :dbi
               :iterate
               :cl-annot
               :cl-fad
               :osicat)
  :components ((:module "src"
                :serial t
                :components
                ((:file "errors")
                 (:file "config")
                 (:module "utils"
                  :components
                  ((:file "utils")))
                 (:file "connect")
                 (:file "sql")
                 (:file "meta")
                 (:file "query")
                 (:file "migration")
                 (:file "table")
                 (:file "interface")
                 (:file "crane"))))
  :description "An ORM for Common Lisp."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op crane-test))))
