(let ((ins (create 'user :name "Eudoxia")))
  ;; CREATE implicity saves the object
  (save ins)
  (del ins))
