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
  (:export :migrate
           :reset
           :seed))

(in-package :uuidnet.db.schema)


(defun migrate ()
  "Run all migrations to set up the database."
  (create-users)
  (create-messages)
  )

(defun reset ()
  "Drop all tables and recreate."
  (drop-all-tables)
  (migrate))

(defun seed ()
  "Create basic test data."

  )


;; --- Individual migrations --- ;;
(defun create-users ()
  "Create the users table."
  (with-connection (db)
    (execute
     (create-table (:user :if-not-exists nil)
         ((id :type 'serial :primary-key t)
          (username :type 'text :not-null t :unique t)
          (email :type 'text :not-null t :unique t)
          (password :type 'text :not-null t)
          (uuid :type 'text :not-null t :unique t)
          (created_at :type 'timestamp :not-null t)
          (updated_at :type 'timestamp :not-null t))))))


(defun create-messages ()
  "Create the messges table."
  (with-connection (db)
    (execute
     (create-table (:message :if-not-exists nil)
         ((id :type 'serial :primary-key t)
          (sender_id :type 'int :not-null t)
          (recipient_id :type 'int :not-null t)
          (body :type 'text :not-null t)
          (created_at :type 'timestamp :not-null t)
          (updated_at :type 'timestamp :not-null t))))))

(defun drop-all-tables ()
  "Drop all tables from the database."

  (with-connection (db)
    (execute
     (drop-table :user :if-exists t)
     )
    (execute
     (drop-table :message :if-exists t)
     ))
  )
