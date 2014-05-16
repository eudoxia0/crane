(defpackage :crane
  (:use :cl)
  (:import-from :crane.config
                :setup)
  (:import-from :crane.connect
                :*default-db*
                :connect
                :get-connection)
  (:import-from :crane.query
                :query)
  (:import-from :crane.migration
                :build
                :delete-migrations)
  (:import-from :crane.table
                :<table>
                :deftable)
  (:import-from :crane.inflate-deflate
                :definflate)
  (:import-from :crane.interface
                :drop-table
                :create
                :save
                :del
                :filter
                :exists
                :single
                :single!
                :single-or-create
                :deref)
  (:import-from :crane.transaction
                :with-transaction
                :commit
                :rollback)
  (:export
   ;; Configuration
   :setup
   ;; Connections
   :*default-db*
   :connect
   :get-connection
   ;; Low-level interface
   :prepare
   :execute
   :query
   ;; Migrations
   :build
   :delete-migrations
   ;; Table macro
   :<table>
   :deftable
   ;; High-level interface
   :drop-table
   :create
   :save
   :del
   :filter
   :exists
   :single
   :single!
   :single-or-create
   :deref))
