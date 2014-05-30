;;;; Automatic

(with-transaction ()
  (let ((restaurants (filter '<restaurant> ...)))
    (loop for restaurant in restaurants do
          ...
          (save restaurant))))

;;;; Manual

(progn
  (begin-transaction)
  (let ((restaurants (filter '<restaurant> ...)))
    (loop for restaurant in restaurants do
          ...
          (save restaurant)))
  (commit))
