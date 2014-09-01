*******************************
Appendix B: How Crane is tested
*******************************

Crane uses Vagrant to define multiple isolated virtual machines. The machines
are provisioned using two scripts: An OS-specific script (Such as
:code:`debian.sh` or :code:`centos.sh`) in the :code:`t/provision` folder, and
then the :code:`common.sh` script in the same folder which handles
OS-independent things such as installing Quicklisp.

The :code:`t/test.sh` file handles setting up the databases (Where necessary,
for example, SQLite3 doesn't need that), running the tests, and then taking down
the databases.

The :code:`run_tests.sh` script in the project root takes care of bringing up
the machines, provisioning them, and running the tests. The virtual machines are
defined in the :code:`Vagrantfile` file.

Tests
=====

The structure of the tests (:code:`t`) folder is:

* :code:`packages.lisp`: Defines the Common Lisp packages for the general and
  database-specific tests.
* :code:`utils.lisp`: Tests the utilities Crane implements for itself.
* :code:`connection-specs.lisp`: Tests the validity or invalidity of various
  connection specifications. See :ref:`Connecting <connecting>`.
* :code:`postgres/`: Contains the tests specific to Postgres.
* :code:`sqlite3/`: Contains SQLite3-specific tests.
