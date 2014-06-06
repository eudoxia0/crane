% Crane Manual
% Fernando Borretti

# Overview

Crane is an ORM for Common Lisp. It's intended to provide a simple,
object-oriented interface to relational databases, inspired by the simplicity of
the Django ORM and the flexible, non-opinionated philosophy of SQLAlchemy.

## Structure

Crane mainly uses two libraries:

[SxQL](https://github.com/fukamachi/sxql)
  ~ A DSL for generating SQL through function composition.
[cl-dbi](https://github.com/fukamachi/cl-dbi)
  ~ A backend-independent interface to relational DBMSs. At
    present, only PostgreSQL, SQLite version 3, and MySQL are
    supported, but support for Oracle and MS SQL Server
    can be added.

# Connecting

Crane autoconnects to all the databases specified in the `:databases` key of the
configuration when the `crane:connect` function is called (No parameters).

Configuration for the databases might look like this:

```lisp
(setup
 :migrations-directory
 (merge-pathnames
  #p"migrations/"
  (asdf:system-source-directory :myapp))
 :databases
 '(:main
   (:type :postgres
    :name "myapp_db"
    :user "user"
    :pass "user")))

(connect)
```

The value of `:databases` is a plist that maps a database's name (Not the actual
name, but rather an identifier, like `:main` or `:users-db`) to a list of
connection parameters, called the *connection spec*.

Crane maintains a list of connection specs for every supported database backend,
and ensures that all required parameters and no parameters other than the
required and optional ones are passed. Connection specs for all supported
backends are listed in [Appendix A: Connecting](#appendix-a-connecting).

# Tables

Crane uses the metaobject protocol to bind SQL tables and CLOS objects through a
`TABLE-CLASS` metaclass. Table classes can be defined simply through the
`deftable` macro, the syntax being:

```lisp
(deftable <name> (<superclass>*)
  <field-or-option>*)
```

For example:

```lisp
(deftable enemy ()
  (name :type string :pk t)
  (age :type integer :check '(:> 'age 12))
  (address :type 'string :nullp t :foreign (important-addresses :cascade :cascade))
  (fatal-weakness :type text :default "None")
  (identifying-color 'type (string 20) :unique t :foreign (colors name)))
```

## Slot Options

`:type`
  ~ The type of the column. No default.

`:nullp`
  ~ Whether the column is nullable or not. Default: False.

`:uniquep`
  ~ Whether the column's values is unique across the table. Default: False.

`:primaryp`
  ~ Whether the column is a primary key of the table. Default: False.

`:indexp`
  ~ Whether the column is an index of the table. Default: False.

`:autoincrementp`
  ~ If true, when adding a new column, this value will be one greater than the
    previous highest value in the table. Table type must be an integer. Default: False.

`:foreign`
  ~ Defines a foreign key. The value of this slot can either be the name of the
    table to point to; or a list where the first element is the name of the
    table to point to, and the next two elements are, respectively, the action
    to perform on deletes and on updates. These are:

- `:cascade`
- `:restrict`
- `:no-action`
- `:set-null`
- `:set-default`

## Table Options

`:table-name`
  ~ A symbol that will be converted to the table's SQL name (Not a string).
    Default: The class's name.

`:abstractp`
  ~ Determines whether the table is abstract. Abstract table only provide slots
    for subclasses to inherit, and don't compile to actual SQL tables.

`:deferredp`
  ~ Deferred classes are only built on demand by calling `crane:build`. Default:
    False.

# Creating, Saving, and Deleting Objects

## `create`

Syntax:
  ~ `(create <class> <params>*)`

Create an instance of a class on the database.

**Examples:**

```lisp
(create 'user :name "Eudoxia")

(create 'company :name "Initech" :founded 1994)
```

## `save`

Syntax:
  ~ `(save <instance>)`

Save an instance's fields to the database.

**Examples:**

```lisp
(let ((point (create 'point :x 556.3 :y 26.7)))
  ;; Make some changes
  (setf (point-distance-from-origin point)
        (euclidean-distance point '(0 0)))
  ;; Save
  (save point))
```

## `del`

Syntax:
  ~ `(del <instance>)`

Delete an instance from the database.

**Examples:**

```lisp
(defun delete-user (username)
  (del (single 'user :name username)))
```

# Making Queries

## High Level API

### `filter`

Syntax:
  ~ `(filter <class> <params>*)`

Return a list of objects that satisfy the `params`.

**Examples:**

```lisp
  (filter 'company :country "US"
                   (:< nemployees 40))
```

### `single`

Syntax:
  ~ `(filter <class> <params>*)`

Return a single object that satisfies the parameters.

A variant, `single!`, will signal a condition when no object satisfies the
parameters.

### `get-or-create`

## Functional SQL

Crane exports the important bits of SxQL so you can write queries using this DSL
without worrying about packages. The syntax is fairly straightforward, and has
few surprises, so a lot of the time consulting the documentation is not
required. It's simply SQL with Lisp syntax.

**Examples:**

```lisp
cl-user> (query (select :tonnage
                  (from :ship)
                  (where (:and (:> :tonnage 125)
                               (:<= :tonnage 500)))
                  (order-by :tonnage)
                  (limit 10)))
;; => ((:|tonnage| 445))
```

# Migrations

Your schema will change, and this is a fact. Most ORMs hope the users will be
happy running manual `ALTER TABLEs` or provide migration functionality through
an external plugin
([Alembic](https://alembic.readthedocs.org/en/latest/front.html) for SQLAlchemy,
[South](http://south.aeracode.org/) for the Django ORM).

Migrations are completely built into Crane, and are designed to be intrusive:
You redefine the schema, reload, and Crane takes care of everything. If your
migration plan is too complicated for Crane, then you write a simple function
that does some transformations and Crane puts that in its migration history, all
that without ever having to leave your Lisp environment or accessing the shell.

## Example

```lisp
(deftable employees
  (name :type string :null nil)
  (age  :type integer)
  (address :type string :null nil))
```

Now, if you decide that addresses can be nullable, you just redefine the class
(Make the change, and either `C-c C-c` on Emacs or Quickload your project):

```lisp
(deftable employees
  (name :type string :null nil)
  (age  :type integer)
  (address :type string))
```

And Crane will spot the difference and perform the migration automatically.

# Transactions

Crane supports a thin wrapper over CL-DBI's transaction capabilities.

## `with-transaction`

Syntax:
  ~ `(with-transaction ([db-name *default-db*]) <body>*)`

Execute `body` inside a transaction. If the code executes, the transaction is
committed. If a condition is signalled, the transaction is rolled back.

**Examples:**

```lisp
  (with-transaction (:my-db)
    (let ((restaurants (filter '<restaurant> ...)))
        (loop for restaurant in restaurants do
            ...
            (save restaurant))))
```

## `begin-transaction`

Syntax:
  ~ `(begin-transaction [db-name *default-db*])`

Start a transaction on the database `db-name`.

## `commit`

Syntax:
  ~ `(commit [db-name *default-db*])`

Commit the current transaction on the database `db-name`.

## `rollback`

Syntax:
  ~ `(rollback [db-name *default-db*])`

Abort the current transaction on the database `db-name`.

# Fixtures

Fixtures are provided through the
[clos-fixtures](https://github.com/eudoxia0/clos-fixtures) library, and can be
used for anything from loading mostly unchanging data (A list of countries, for
example) to setting up massive datasets for testing.

**Examples:**

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

# Inflate/Deflate

Crane supports *inflating* values returned by the database into more complex
CLOS objects, and *deflating* those same objects back to an SQL
representation. This can be useful for accessing database extensions that
provide complex types for columns, like Postgres'
[PostGIS](http://postgis.net/).

**Examples:**

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

# Appendix A: Connecting

## PostgreSQL

**Required:**

`:name`
  ~ Database name.
`:user`
  ~ User name.
`:pass`
  ~ User password.

**Optional:**

`:host`
  ~ Host that runs the database server. Default: `localhost`.
`:port`
  ~ Port the database server listens on. Default: `5432`.
`:ssl`
  ~ `:yes` enables secure SSL connections to the server. This might be useful if
  the server is running on a host other than the default. Note that OpenSSL must
  be installed on both machines. For mpre information, see the
  [relevant PostgreSQL documentation](http://www.postgresql.org/docs/9.1/static/ssl-tcp.html). Default: `:no`.

## SQLite

**Required:**

`:name`
  ~ The name of the database. As usual, a value of `:memory:` will create an
    in-memory database.

## MySQL

**Required and Optional**: Same as [PostgreSQL](#postgresql), except for
`:ssl`. The default port number `3306`.
