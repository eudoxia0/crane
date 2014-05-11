(defsystem crane-docs
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Generate documentation."
  :defsystem-depends-on (:asdf-linguist)
  :components ((:module "docs"
                :serial t
                :components
                ((:module "img"
                  :components
                  ((:ditaa "dia")))
                 (:pandoc "manual"
                  :type "md"
                  :input-type "markdown"
                  :output "manual"
                  :output-type "html5"
                  :output-extension "html"
                  :options "-S")))))
