(in-package :crane-test)

(setf crane.connect::*db* (crane.connect::initialize-*db*))

(run! 'preliminary)

(run! 'crane-test.postgres:postgres)
