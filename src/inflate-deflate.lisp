(in-package :cl-user)
(defpackage crane.inflate-deflate
  (:use :cl :anaphora :crane.types)
  (:export :inflate
           :definflate
           :deflate
           :defdeflate)
  (:documentation "Inflation/deflation map SQL string to CLOS objects. This is
unrelated to the ORM, and meant to allow complex column datatypes to be mapped
to CLOS objects. For example, mapping SQL timestamps to structures that
represent time, or mapping other more complex SQL types to CLOS objects."))
(in-package :crane.inflate-deflate)

(defgeneric deflate (obj)
  (:documentation "Turn a Lisp object into a string for insertion in the
  database."))

(defmacro defdeflate ((obj-name obj-type) &rest body)
  `(defmethod crane.inflate-deflate:deflate ((,obj-name ,obj-type))
     ,@body))

(defdeflate (str string) str)
(defdeflate (num number) num)
(defdeflate (obj t) obj)

(defgeneric inflate (obj type-name)
  (:documentation "Turn a string into a CLOS object."))

(defmacro definflate ((obj-name obj-type-name) &rest body)
  `(defmethod crane.inflate-deflate:inflate (,obj-name
                                             (type (eql ,obj-type-name)))
     ,@body))

(definflate (obj 'integer) obj)
(definflate (obj 'bigint) obj)
(definflate (obj 'smallint) obj)
(definflate (obj 'numeric) obj)
(definflate (obj 'double) obj)
(definflate (obj 'text) obj)
(definflate (obj 'varchar) obj)
(definflate (obj 'datetime) obj)

(definflate (stamp 'timestamp)
    (if (integerp stamp)
        (local-time:universal-to-timestamp stamp)
        (local-time:parse-timestring stamp)))

(defdeflate (stamp local-time:timestamp)
    (local-time:format-timestring nil stamp :timezone local-time:+utc-zone+))
