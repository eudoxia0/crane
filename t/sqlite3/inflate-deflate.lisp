(in-package :crane-test.sqlite3)

(test create-table
  (finishes
   (deftable sq-table-with-time ()
     (field :type timestamp))))

(test (queries :depends-on create-table)
  (is-true
   (let* ((the-timestamp (local-time:today))
          (instance (create 'sq-table-with-time
                            :field the-timestamp))
          (found-instance
	   (single 'sq-table-with-time
		   :field
		   (crane.inflate-deflate:deflate the-timestamp))))
     (declare (ignore instance))
     (local-time:timestamp= the-timestamp
                            (field found-instance)))))
