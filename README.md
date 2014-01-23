Crane is an ORM for Common Lisp, providing a simple bridge between CLOS and
relational databases, and out of the box migrations.

# Usage

## Defining Tables

```lisp
(deftable user ()
  (name :type text :uniquep t)
  (age :type integer :nullp nil :initform 18))
```

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

Crane uses [Envy](https://github.com/fukamachi/envy) for configuration
management.

```lisp
(setf (config-env-var) "MYAPP_ENV")

(defconfig |conf|
  `(:crane (:migrations-directory
             ,(merge-pathnames
               #p"t/migrations/"
               (asdf:component-pathname (asdf:find-system :crane-test)))
            :databases
              (:main
               (:type :postgres
                :name "myapp_db"
                :user "user"
                :pass "pass")))))

(setf (env-var "MYAPP_ENV") "conf")

(crane:connect)
```

# Creating, Saving, and Deleting Objects

```lisp
(let ((ins (create 'user :name "Eudoxia")))
  (save ins)
  (del ins))
```

# Filtering

```lisp
(filter 'user) ;; Returns everything

(filter 'user :name "Eudoxia")

(filter 'user (:> :age 21))

(single 'user :name "Eudoxia") ;; Returns a single object

(single! 'user (:< age 35)) ;; Throws an error if this returns more than one
                            ;; object

(exists 'user :name "Eudoxia") ;; t if a match exists, nil otherwise

(get-or-create 'user :name "Eudoxia" :age 19) ;; If this record doesn't exist
                                              ;; create it
```

# Documentation

The manual is provided in [org-mode](http://orgmode.org/) format in
`docs/manual.org`. It can be compiled to HTML or PDF, or you can grab a copy
from the site (Soon).

# Notes

Testing requires Postgres and running `t/setup.sh`.

# License

Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com)

Released under the MIT license.
