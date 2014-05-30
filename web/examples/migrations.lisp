(deftable ship ()
  (name :type string)
  (flag :type string)
  (tonnage :type integer))

;; Oops

(deftable ship ()
  (name :type string :indexp t)
  (flag :type string :nullp nil)
  (tonnage :type integer))
