(filter 'user) ;; Returns everything

(filter 'user :name "Eudoxia")

(filter 'user (:> :age 21))

;; Returns a single object
(single 'user :name "Eudoxia")

;; Throws an error if this returns more
;; than one object
(single! 'user (:< age 35))

;; t if a match exists, nil otherwise
(exists 'user :name "Eudoxia")

;; If this record doesn't exist create it
(get-or-create 'user :name "Eudoxia" :age 19)
