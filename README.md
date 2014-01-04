Crane is an ORM for Common Lisp, providing a simple bridge between CLOS and
relational databases, and out of the box migrations.

# Usage

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
```

## Defining Tables

```lisp
(deftable user
  (name :type text :uniquep t)
  (age :type integer :nullp nil))
```

# License

Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com)

Released under the MIT license.
