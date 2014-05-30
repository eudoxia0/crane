;;;; initial-data.lisp
(app:user
  (:name "eudoxia"
   :groups (:admin :staff))
  (:name "joe"
   :groups (:admin)))
(app:company
  (:name "Initech"
   :city "Denver"))

;;;; myapp.asd
(asdf:defsystem myapp
  :defsystem-depends-on (:clos-fixtures)
  :components ((:module "src"
                :components
                ((:fixture "initial-data")))))
