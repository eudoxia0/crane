(in-package :cl-user)
(defpackage crane-asd
  (:use :cl :asdf))
(in-package :crane-asd)

(defsystem crane
  :version "0.1"
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "LLGPL"
  :depends-on (:closer-mop
               :cl-json
               :anaphora
               :s-sql
               :envy)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "meta")
                 (:file "table"))))
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
