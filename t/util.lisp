(in-package :crane-test.util)

(def-suite util)
(in-suite util)

(test diff-plist
  (is
   (equal
    (crane.utils:diff-plist '(:a 1 :b 2 :c 3) '(:a 1 :b 2 :c 3))
    nil))
  (is
   (equal
    (crane.utils:diff-plist '(:a 1 :b 2 :c 3) '(:a 4 :b 5 :c 6))
    '(:a (1 4) :b (2 5) :c (3 6)))))
