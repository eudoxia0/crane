---
title: Tables
layout: docs
---

Crane uses the metaobject protocol to bind SQL tables and CLOS objects through a
`TABLE-CLASS` metaclass. Table classes can be defined simply through the
`deftable` macro, the syntax being:

~~~lisp
(deftable <name> (<superclass>*)
  <field-or-option>*)
~~~

For example:

~~~lisp
(deftable enemy ()
  (name :type text :primaryp t)
  (age :type integer :check (:> 'age 12))
  (address :nullp t
           :foreign (important-addresses :cascade :cascade))
  (fatal-weakness :type text :default "None")
  (identifying-color :unique t :foreign (colors name)))
~~~

## Slot Options

`:type`
: The type of the column. No default.

`:nullp`
: Whether the column is nullable or not. Default: False.

`:uniquep`
: Whether the column's values is unique across the table. Default: False.

`:primaryp`
: Whether the column is a primary key of the table. Default: False.

`:indexp`
: Whether the column is an index of the table. Default: False.

`:autoincrementp`
: If true, when adding a new column, this value will be one greater than the
  previous highest value in the table. Table type must be an integer. Default:
  False.

`:foreign`
: Defines a foreign key. The value of this slot can either be the name of the
  table to point to; or a list where the first element is the name of the table
  to point to, and the next two elements are, respectively, the action to
  perform on deletes and on updates. These are:
  * `:cascade`
  * `:restrict`
  * `:no-action`
  * `:set-null`
  * `:set-default`

## Table Options

`:table-name`
: A symbol that will be converted to the table's SQL name (Not a string).
  Default: The class's name.

`:abstractp`
: Determines whether the table is abstract. Abstract table only provide slots
  for subclasses to inherit, and don't compile to actual SQL tables.

`:deferredp`
: Deferred classes are only built on demand by calling `crane:build`. Default:
  False.
