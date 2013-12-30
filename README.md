Crane is an ORM for Common Lisp, providing a simple bridge between CLOS and relational databases, and out of the box migrations.

# Usage

```lisp
(deftable user
  (name :type text :uniquep t)
  (age :type integer :nullp nil))
```

# License

Copyright (c) 2013 Fernando Borretti (eudoxiahp@gmail.com)

Released under the MIT license.
