********
Fixtures
********

Fixtures are provided through the `clos-fixtures`_ library, and can be used for
anything from loading mostly unchanging data (A list of countries, for example)
to setting up massive datasets for testing.

Examples:

::

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

.. _clos-fixtures: https://github.com/eudoxia0/clos-fixtures
