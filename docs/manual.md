% Crane Manual
% Fernando Borretti

# Overview

Crane is an ORM for Common Lisp. It's intended to provide a simple,
object-oriented interface to relational databases, inspired by the simplicity of
the Django ORM and the flexible, non-opinionated philosophy of SQLAlchemy.

## Structure

![Structure](img/dia.png)

Crane mainly uses two libraries:

[sxql](https://github.com/fukamachi/sxql)
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
     :pass "crane_test_user")
    :interface
    (:type :sqlite3
     :name ":memory:"))))
```

The value of `:databases` is a plist that maps a database's name (Not that
actual name, but rather a way to identify it, like `:main` or `:users-db`) to a
list of connection parameters, called the *connection spec*.

Crane maintains a list of connection specs for every supported database backend,
and ensures that all required parameters and no parameters other than the
required and optional ones are passed. Connection specs for all supported
backends are listed in [Appendix A: Connecting](#appendix-a-connecting).

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
  ~ Host that runs the database server. Default: "=localhost=".
`:port`
  ~ Port the database server listens on. Default: =5432=.
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
=:ssl=. The default port number =3306=.
