(in-package :crane-test)

(def-suite util-tests)
(in-suite util-tests)

(test diff-plist
  (is (equal
       (crane.util:diff-plist '(:a 1 :b 2 :c 3) '(:a 1 :b 2 :c 3))
       nil)
      (equal
       (crane.util:diff-plist '(:a 1 :b 2 :c 3) '(:a 4 :b 5 :c 6))
       '(:a (1 4) :b (2 5) :c (3 6)))))

(run! 'util-tests)
