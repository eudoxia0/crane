(defsystem crane-web
  :version "0.1"
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Generate a static site for the project."
  :defsystem-depends-on (:asdf-linguist)
  :depends-on (:cl-markup)
  :components ((:module "web"
                :serial t
                :components
                ((:file "gen")))))
