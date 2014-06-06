(deftable ship ()
  (name :type text)
  (flag :type text)
  (tonnage :type integer))

;; Oops

(deftable ship ()
  (name :type text :indexp t)
  (flag :type text :nullp nil)
  (tonnage :type integer))
