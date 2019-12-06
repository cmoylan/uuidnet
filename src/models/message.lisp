(in-package :cl-user)

(defpackage uuidnet.models.message
  (:use :cl :sxql)
  (:import-from :uuidnet.db
                :db
                :with-connection
                :with-transaction)
  (:import-from :datafly
                :defmodel
                :execute
                :retrieve-all
                :retrieve-one)
  (:export ))

(in-package :uuidnet.models.message)

(defmodel message
  id)
