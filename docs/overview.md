---
title: Overview
layout: docs
---

Crane is an ORM for Common Lisp. It's intended to provide a simple,
object-oriented interface to relational databases, inspired by the simplicity of
the Django ORM and the flexible, non-opinionated philosophy of SQLAlchemy.

# Structure

Crane mainly uses two libraries:

[`SxQL`][sxql]
: A DSL for generating SQL through function composition.

[`cl-dbi`][dbi]
: A backend-independent interface to relational DBMSs. At present, only
  PostgreSQL, SQLite version 3, and MySQL are supported, but support for Oracle
  and MS SQL Server can be added.

[sxql]: https://github.com/fukamachi/sxql
[dbi]: https://github.com/fukamachi/cl-dbi
