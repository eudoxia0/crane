(deftable user ()
  (name :type text :uniquep t)
  (age  :type integer :indexp t)
  (company :foreign-key 'company :nullp nil))

(loop for user in (filter 'user (:< :age 25)) do
  (format t "Company: ~A~&"
          (name (deref user 'company))))
