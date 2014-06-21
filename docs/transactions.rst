************
Transactions
************

Crane supports a thin wrapper over CL-DBI's transaction capabilities.

:code:`with-transaction`
========================

Syntax:
   :code:`(with-transaction ([db-name *default-db*]) <body>*)`

Execute `body` inside a transaction. If the code executes, the transaction is
committed. If a condition is signalled, the transaction is rolled back.

Examples:

::

  (with-transaction (:my-db)
    (let ((restaurants (filter '<restaurant> ...)))
        (loop for restaurant in restaurants do
            ...
            (save restaurant))))

:code:`begin-transaction`
=========================

Syntax:
   :code:`(begin-transaction [db-name *default-db*])`

Start a transaction on the database :code:`db-name`.

:code:`commit`
==============

Syntax:
   :code:`(commit [db-name *default-db*])`

Commit the current transaction on the database :code:`db-name`.

:code:`rollback`
================

Syntax:
   :code:`(rollback [db-name *default-db*])`

Abort the current transaction on the database :code:`db-name`.
