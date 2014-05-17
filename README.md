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

```lisp
(crane:setup
 `(:migrations-directory
   ,(merge-pathnames
     #p"t/migrations/"
     (asdf:component-pathname (asdf:find-system :crane-test)))
   :databases
   (:main
    (:type :postgres
     :name "crane_test_db"
     :user "crane_test_user"
     :pass "crane_test_user"))))

(crane:connect)
```

For configuration management and switching databases in development/production
environments, you might want to use [Envy](https://github.com/fukamachi/envy).

## Creating, Saving, and Deleting Objects

```lisp
(let ((ins (create 'user :name "Eudoxia")))
  ;; CREATE implicity saves the object
  ;; <some changes here>
  (save ins)
  (del ins))
```

## Filtering

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

The manual is provided in Markdown format in `docs/manual.md`. A document on the
internals is also provided. The `crane-docs` system compiles those to PDF, and
`crane-web` compiles them to HTML in addition to generating the site.

# Notes

Testing requires Postgres and running `t/setup.sh`.

# License

Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com)

Released under the MIT license.
