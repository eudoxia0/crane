(defsystem crane-docs
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Generate documentation in PDF format."
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
                  :output-type "pdf"
                  :output-extension "pdf")
                 (:pandoc "internals"
                  :type "md"
                  :input-type "markdown"
                  :output "internals"
                  :output-type "pdf"
                  :output-extension "pdf")))))
