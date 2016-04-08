(in-package :crane-test)

(setf crane.connect::*db* (crane.connect::initialize-*db*))

(run! 'preliminary)

(run! 'crane-test.sqlite3:sqlite3)
