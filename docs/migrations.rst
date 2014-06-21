**********
Migrations
**********

Your schema will change, and this is a fact. Most ORMs hope the users will be
happy running manual `ALTER TABLEs` or provide migration functionality through
an external plugin (`Alembic`_ for SQLAlchemy, `South`_ for the Django ORM).

Migrations are completely built into Crane, and are designed to be intrusive:
You redefine the schema, reload, and Crane takes care of everything. If your
migration plan is too complicated for Crane, then you write a simple function
that does some transformations and Crane puts that in its migration history, all
that without ever having to leave your Lisp environment or accessing the shell.

.. _Alembic: https://alembic.readthedocs.org/en/latest/front.html
.. _South: http://south.aeracode.org/

Example
=======

::

  (deftable employees
    (name :type string :null nil)
    (age  :type integer)
    (address :type string :null nil))

Now, if you decide that addresses can be nullable, you just redefine the class
(Make the change, and either `C-c C-c` on Emacs or Quickload your project):

::

  (deftable employees
    (name :type string :null nil)
    (age  :type integer)
    (address :type string))

And Crane will spot the difference and perform the migration automatically.
