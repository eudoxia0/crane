********
Overview
********

Crane is an ORM for Common Lisp. It's intended to provide a simple,
object-oriented interface to relational databases, inspired by the simplicity of
the Django ORM and the flexible, non-opinionated philosophy of SQLAlchemy.

Structure
=========

Crane mainly uses two libraries:

`SxQL`_
  A DSL for generating SQL through function composition.

`cl-dbi`_
   A backend-independent interface to relational DBMSs. At present, only
   PostgreSQL, SQLite version 3, and MySQL are supported, but support for Oracle
   and MS SQL Server can be added.

.. _SxQL: https://github.com/fukamachi/sxql
.. _cl-dbi: https://github.com/fukamachi/cl-dbi
