(defpackage :crane
  (:use :cl :crane.types)
  (:import-from :crane.config
                :setup)
  (:import-from :crane.connect
                :*default-db*
                :connect
                :get-connection)
  (:import-from :crane.query
                :query
                :do-query)
  (:import-from :crane.migration
                :build
                :delete-migrations)
  (:import-from :crane.table
                :<table>
                :deftable)
  (:import-from :crane.inflate-deflate
                :definflate
                :defdeflate)
  (:import-from :crane.interface
                :drop-table
                :create
                :create-from-plist
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
                :begin-transaction
                :commit
                :rollback)
  ;; Import a whole bunch of SxQL stuff
  (:import-from :sxql
                :compose-statements
                :delete-from
                :fields
                :from
                :full-join
                :group-by
                :inner-join
                :join
                :left-join
                :limit
                :offset
                :order-by
                :right-join
                :select
                :union-all-queries
                :union-queries
                :where
                :yield)
  (:export
   ;; Configuration
   :setup
   ;; Connections
   :*default-db*
   :connect
   :get-connection
   ;; Low-level interface
   :query
   :do-query
   ;; Migrations
   :build
   :delete-migrations
   ;; Table macro
   :<table>
   :deftable
   ;; Types
   :int
   :bigint
   :smallint
   :numeric
   :double
   :text
   :varchar
   :timestamp
   :datetime
   ;; Inflate/deflate
   :definflate
   :defdeflate
   ;; High-level interface
   :drop-table
   :create
   :create-from-plist
   :save
   :del
   :filter
   :exists
   :single
   :single!
   :single-or-create
   :deref
   ;; Transactions
   :with-transaction
   :begin-transaction
   :commit
   :rollback
   ;; SxQL
   :compose-statements
   :delete-from
   :fields
   :from
   :full-join
   :group-by
   :inner-join
   :join
   :left-join
   :limit
   :offset
   :order-by
   :right-join
   :select
   :union-all-queries
   :union-queries
   :where
   :yield))
