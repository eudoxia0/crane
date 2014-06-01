cl-user> (query (select :tonnage
                  (from :ship)
                  (where (:and (:> :tonnage 125)
                               (:<= :tonnage 500)))
                  (order-by :tonnage)
                  (limit 10)))
;; => ((:|tonnage| 445))
