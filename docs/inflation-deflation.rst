***************
Inflate/Deflate
***************

Crane supports *inflating* values returned by the database into more complex
CLOS objects, and *deflating* those same objects back to an SQL
representation. This can be useful for accessing database extensions that
provide complex types for columns, like Postgres' `PostGIS`_.

Examples:

::

  (definflate (stamp 'timestamp)
    ;; Inflate a timestamp value
    ;; into a timestamp object
    (local-time:universal-to-timestamp stamp))

  (defdeflate (stamp local-time:timestamp)
    ;; Deflate a timestamp object
    ;; into a string
    (local-time:format-timestring nil stamp))

.. _Postgis: http://postgis.net/
