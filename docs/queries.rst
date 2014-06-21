**************
Making Queries
**************

High Level API
==============

:code:`filter`
--------------

Syntax
   :code:`(filter <class> <params>*)`

Return a list of objects that satisfy the :code:`params`.

Examples:

::

  (filter 'company :country "US"
                   (:< nemployees 40))

:code:`single`
--------------

Syntax:
   :code:`(filter <class> <params>*)`

Return a single object that satisfies the parameters.

A variant, :code:`single!`, will signal a condition when no object satisfies the
parameters.

:code:`get-or-create`
---------------------

Functional SQL
==============

Crane exports the important bits of SxQL so you can write queries using this DSL
without worrying about packages. The syntax is fairly straightforward, and has
few surprises, so a lot of the time consulting the documentation is not
required. It's simply SQL with Lisp syntax.

Examples:

::

  cl-user> (query (select :tonnage
                    (from :ship)
                    (where (:and (:> :tonnage 125)
                                 (:<= :tonnage 500)))
                    (order-by :tonnage)
                    (limit 10)))
  ;; => ((:|tonnage| 445))
