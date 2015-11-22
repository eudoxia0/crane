# Crane

[![Build Status](https://travis-ci.org/eudoxia0/crane.svg?branch=master)](https://travis-ci.org/eudoxia0/crane)
[![Quicklisp](http://quickdocs.org/badge/crane.svg)](http://quickdocs.org/crane/)

Crane is an ORM for Common Lisp, providing a simple bridge between CLOS and
relational databases, and out of the box migrations.

# Usage

## Defining Tables

```lisp
(deftable user ()
  (name :type text :uniquep t)
  (age :type integer :nullp nil :initform 18)
  (friend :type integer :foreign user)))
```
The foreign argument accepts a symbol that represents another table or a sexp of the form `(table &key on-delete on-update))`, where acceptable values are `:no-action :restrict :cascade :set-null :set-default`.

## Migrating

```lisp
(deftable user ()
  (name :type text :uniquep t :nullp nil)
  (age :type integer :nullp t :initform 18)
  (description :type text))
```

Just make the changes, and Crane will compute the diffs and perform all the
`ALTER TABLE`s for you.

## Connecting

```lisp
(setup
 :migrations-directory
 (asdf:system-relative-pathname :myapp #p"migrations/")
 :databases
 '(:main
   (:type :postgres
    :name "myapp_db"
    :user "user"
    :pass "user")))

(connect)
```

For configuration management and switching databases in development/production
environments, you might want to use [Envy](https://github.com/fukamachi/envy).

## Creating, Saving, and Deleting Objects

```lisp
(let ((instance (create 'ship :name "Dalliance"
                              :tonnage 77)))
  ;; FIXME: It's back luck to rename a ship
  (setf (name instance) "Serenity")
  ;; Expand the cargo hold
  (incf (tonnage instance) 25)
  ;; Save these changes!
  (save instance)
  ;; Time to retire
  (del instance))
```

## Filtering

```lisp
(filter 'user) ;; Returns everything

(filter 'user :name "Eudoxia")

(filter 'user (:> :age 21))

;; Returns a single object
(single 'user :name "Eudoxia")

;; Throws an error if this returns more
;; than one object
(single! 'user (:< age 35))

;; t if a match exists, nil otherwise
(exists 'user :name "Eudoxia")

;; If this record doesn't exist create it
(get-or-create 'user :name "Eudoxia" :age 19)
```

## Transactions

```lisp
;;;; Automatic
(with-transaction ()
  (let ((restaurants (filter 'restaurant ...)))
    (loop for restaurant in restaurants do
          ...
          (save restaurant))))

;;;; Manual
(progn
  (begin-transaction)
  (let ((restaurants (filter 'restaurant ...)))
    (loop for restaurant in restaurants do
          ...
          (save restaurant)))
  (commit))
```

## Fixtures

```lisp
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
```

## Inflate/Deflate

```lisp
(definflate (stamp 'timestamp)
  ;; Inflate a timestamp value
  ;; into a timestamp object
  (local-time:universal-to-timestamp stamp))

(defdeflate (stamp local-time:timestamp)
  ;; Deflate a timestamp object
  ;; into a string
  (local-time:format-timestring nil stamp))
```

# Documentation

I'm in the process of moving the documentation to [Codex][codex], so for now you
can check it out in the [website][docs-pdf].

[codex]: https://github.com/CommonDoc/codex
[docs-pdf]: http://eudoxia.me/crane/docs/manual.pdf

# License

Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com)

Released under the MIT license.
