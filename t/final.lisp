(in-package :crane-test.final)

(run! 'crane-test.util:util-tests)

(run! 'crane-test.specpostgres)
(run! 'crane-test.specmysql)
(run! 'crane-test.specsqlite3)
