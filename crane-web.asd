(defsystem crane-web
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Generate a static site for the project, and HTML docs."
  :defsystem-depends-on (:asdf-linguist)
  :depends-on (:crane-docs
               :cl-markup)
  :components ((:module "docs"
                :serial t
                :components
                (;; The ditaa diagram is taken care of by crane-docs
                 (:pandoc "manual"
                  :type "md"
                  :input-type "markdown"
                  :output "manual"
                  :output-type "html5"
                  :output-extension "html")
                 (:pandoc "internals"
                  :type "md"
                  :input-type "markdown"
                  :output "internals"
                  :output-type "html5"
                  :output-extension "html")))
               (:module "web"
                :serial t
                :components
                ((:sass "style")
                 (:file "gen")))))
