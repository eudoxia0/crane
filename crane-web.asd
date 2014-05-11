(defsystem crane-web
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Generate a static site for the project."
  :defsystem-depends-on (:asdf-linguist)
  :depends-on (:crane-docs)
  :components ((:module "web"
                :serial t
                :components
                ((:sass "style")
                 (:file "gen")))))
