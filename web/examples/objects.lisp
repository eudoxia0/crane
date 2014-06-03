(let ((instance (create 'ship :name "Dalliance"
                              :tonnage "77")))
  ;; FIXME: It's back luck to rename a ship
  (setf (name instance) "Serenity")
  ;; Expand the cargo hold
  (incf (tonnage instance) 25)
  ;; Save these changes!
  (save instance)
  ;; Time to retire
  (del instance))
