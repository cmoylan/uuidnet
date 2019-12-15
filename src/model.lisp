(in-package :cl-user)

(defpackage uuidnet.model
  (:use :cl :sxql :uuid)
  (:import-from :uuidnet.db
                :db
                :with-connection
                :with-transaction)
  (:import-from :datafly
                :execute
                :retrieve-all
                :retrieve-one))

(in-package :uuidnet.model)

