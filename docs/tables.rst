******
Tables
******

Crane uses the metaobject protocol to bind SQL tables and CLOS objects through a
:code:`TABLE-CLASS` metaclass. Table classes can be defined simply through the
:code:`deftable` macro, the syntax being:

::

  (deftable <name> (<superclass>*)
    <field-or-option>*)

For example:

::

  (deftable enemy ()
    (name :type text :primaryp t)
    (age :type integer :check (:> 'age 12))
    (address :nullp t
             :foreign (important-addresses :cascade :cascade))
    (fatal-weakness :type text :default "None")
    (identifying-color :unique t :foreign (colors name)))

Slot Options
============

:code:`:type`
   The type of the column. No default.

:code:`:nullp`
   Whether the column is nullable or not. Default: False.

:code:`:uniquep`
   Whether the column's values is unique across the table. Default: False.

:code:`:primaryp`
   Whether the column is a primary key of the table. Default: False.

:code:`:indexp`
   Whether the column is an index of the table. Default: False.

:code:`:autoincrementp`
   If true, when adding a new column, this value will be one greater than the
   previous highest value in the table. Table type must be an integer. Default:
   False.

:code:`:foreign`
   Defines a foreign key. The value of this slot can either be the name of the
   table to point to; or a list where the first element is the name of the table
   to point to, and the next two elements are, respectively, the action to
   perform on deletes and on updates. These are:
      * :code:`:cascade`
      * :code:`:restrict`
      * :code:`:no-action`
      * :code:`:set-null`
      * :code:`:set-default`

Table Options
=============

:code:`:table-name`
   A symbol that will be converted to the table's SQL name (Not a string).
   Default: The class's name.

:code:`:abstractp`
   Determines whether the table is abstract. Abstract table only provide slots
   for subclasses to inherit, and don't compile to actual SQL tables.

:code:`:deferredp`
   Deferred classes are only built on demand by calling :code:`crane:build`. Default:
   False.
