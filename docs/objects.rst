**************************************
Creating, Saving, and Deleting Objects
**************************************

:code:`create`
==============

Syntax
   :code:`(create <class> <params>*)`

Create an instance of a class on the database.

Examples:

::

  (create 'user :name "Eudoxia")

  (create 'company :name "Initech" :founded 1994)

:code:`save`
============

Syntax
   :code:`(save <instance>)`

Save an instance's fields to the database.

Examples:

::

  (let ((point (create 'point :x 556.3 :y 26.7)))
    ;; Make some changes
    (setf (point-distance-from-origin point)
          (euclidean-distance point '(0 0)))
    ;; Save
    (save point))

:code:`del`
===========

Syntax
   :code:`(del <instance>)`

Delete an instance from the database.

Examples:

::

  (defun delete-user (username)
    (del (single 'user :name username)))
