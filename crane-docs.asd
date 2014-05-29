(defsystem crane-docs
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Generate documentation in PDF format."
  :defsystem-depends-on (:asdf-linguist)
  :components ((:module "docs"
                :serial t
                :components
                ((:pandoc "manual"
                  :type "md"
                  :input-type "markdown"
                  :output "manual"
                  :output-extension "pdf")
                 (:pandoc "internals"
                  :type "md"
                  :input-type "markdown"
                  :output "internals"
                  :output-extension "pdf")))))
