(in-package :crane-test.postgres)

(def-suite inflate-deflate
  :description "Test inflation/deflation.")
(in-suite inflate-deflate)

(test create-table
  (finishes
   (deftable table-with-time ()
     (field :type timestamp))))

(test definitions
  (finishes
    (definflate (stamp 'timestamp)
        (local-time:universal-to-timestamp stamp))
    (defdeflate (stamp local-time:timestamp)
        (local-time:format-timestring nil stamp))))

(test queries
  (is-true
   (let* ((timestamp (local-time:today))
          (instance (create 'table-with-time
                            :field timestamp))
          (found-instance
            (single 'table-with-time :field timestamp)))
     (local-time:timestamp= timestamp
                            (field found-instance)))))

(run! 'inflate-deflate)
