(in-package :crane-test.postgres)

(def-suite connecting
    :description "Test that we can actually connect.")
(in-suite connecting)

(test connect
  (finishes
    (crane.connect:connect)))

(test main-db
  (is (equal :main crane.connect:*default-db*)))

(run! 'connecting)
