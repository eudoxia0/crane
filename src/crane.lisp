(defpackage :crane
  (:use :cl)
  (:import-from :crane.connect
                :*default-db*
                :connect
                :get-connection)
  (:import-from :crane.query
                :prepare
                :execute
                :query)
  (:import-from :crane.migration
                :build)
  (:import-from :crane.table
                :<table>
                :deftable)
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
  (:export
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
