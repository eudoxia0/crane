(in-package :crane-test.postgres)

(test create-table
  (finishes
   (deftable table-with-time ()
     (field :type timestamp))))

(test (definitions :depends-on create-table)
  (finishes
    (definflate (stamp 'timestamp)
        (local-time:universal-to-timestamp stamp))
    (defdeflate (stamp local-time:timestamp)
        (local-time:format-timestring nil stamp))))

(test (queries :depends-on definitions)
  (is-true
   (let* ((timestamp (local-time:today))
          (instance (create 'table-with-time
                            :field timestamp))
          (found-instance
            (single 'table-with-time :field timestamp)))
     (local-time:timestamp= timestamp
                            (field found-instance)))))
