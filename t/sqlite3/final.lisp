(in-package :crane-test)

(run! 'preliminary)
(run! 'crane-test.sqlite3:config)
(run! 'crane-test.sqlite3:tables)
(run! 'crane-test.sqlite3:queries)
