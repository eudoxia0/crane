(filter 'user) ;; Returns everything

(filter 'user :name "Eudoxia")

(filter 'user (:> :age 21))

(single 'user :name "Eudoxia") ;; Returns a single object

(single! 'user (:< age 35)) ;; Throws an error if this returns more than one
                            ;; object

(exists 'user :name "Eudoxia") ;; t if a match exists, nil otherwise

(get-or-create 'user :name "Eudoxia" :age 19) ;; If this record doesn't exist
                                              ;; create it
