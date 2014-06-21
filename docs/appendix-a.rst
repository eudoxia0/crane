.. _appendix-connecting:

**********************
Appendix A: Connecting
**********************

.. _postgresql:

PostgreSQL
==========

**Required:**

:code:`:name`
   Database name.
:code:`:user`
   User name.
:code:`:pass`
   User password.

**Optional:**

:code:`:host`
   Host that runs the database server. Default: `localhost`.
:code:`:port`
   Port the database server listens on. Default: `5432`.
:code:`:ssl`
   :code:`:yes` enables secure SSL connections to the server. This might be
   useful if the server is running on a host other than the default. Note that
   OpenSSL must be installed on both machines. For mpre information, see the
   relevant `PostgreSQL documentation`_. Default: :code:`:no`.

.. _PostgreSQL documentation:
   http://www.postgresql.org/docs/9.1/static/ssl-tcp.html

SQLite
======

**Required:**

:code:`:name`
   The name of the database. As usual, a value of :code:`:memory:` will create
   an in-memory database.

MySQL
=====

**Required and Optional**: Same as :ref:`PostgreSQL <postgresql>`, except for
:code:`:ssl`. The default port number `3306`.
