(setup
 :migrations-directory
 (merge-pathnames
  #p"migrations/"
  (asdf:system-source-directory :myapp))
 :databases
 '(:main
   (:type :postgres
    :name "myapp_db"
    :user "user"
    :pass "user")))

(connect)
