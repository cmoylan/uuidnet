(in-package :cl-user)

(defpackage uuidnet.db.schema
  (:use :cl :sxql)
  (:import-from :uuidnet.db
                :db
                :with-connection
                :with-transaction)
  (:import-from :datafly
                :execute
                :retrieve-all
                :retrieve-one)
  (:export :migrate))

(in-package :uuidnet.db.schema)

(defun migrate ()
  "Run all migrations to set up the database"
  (with-connection (db)
    (execute
     (create-table (:user :if-not-exists nil)
         ((id :type 'serial :primary-key t)
          (username :type 'text :not-null t :unique t)
          (email :type 'text :not-null t :unique t)
          (password :type 'text :not-null t)
          (uuid :type 'text :not-null t :unique t)
          (created_at :type 'timestamp :not-null t)
          (updated_at :type 'timestamp :not-null t))))

    (execute
     (create-table (:message :if-not-exists nil)
         ((id :type 'serial :primary-key t)
          (sender_id :type 'int :not-null t :references '(:user :id))
          (recipient_id :type 'int :not-null t :references '(:user :id))
          (body :type 'text :not-null t)
          (created_at :type 'timestamp :not-null t)
          (updated_at :type 'timestamp :not-null t))))))
