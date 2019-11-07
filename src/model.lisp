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
                :retrieve-one)
  (:export :create-tables
           :all-users
           :find-user
           :for-template
           :add-user
           :authenticate-user
           :add-page
           :get-latest-page
           :get-latest-pages-by-user
           :get-sorted-pages
           :count-pages
           :nth-page-revision))

(in-package :uuidnet.model)

(defstruct user
  id
  username
  uuid
  email
  password
)

(defun for-template (user)
  "transform the user struct into an alist"
  (list :email (user-email user)
    :uuid (user-uuid user)
    :username (user-username user)))


(defun all-users ()
  "return all users."
  (with-connectoin (db)
    (retrieve-all
     (select :*
       (from :user))
     :as 'user)))

(defun add-user (username email password)
  "add user record to database."
  (with-connection (db)
    (execute
     (insert-into :user
       (set= :username username
             :email email
             :password (cl-pass:hash password)
             :uuid (generate-uuid))))))

(defun generate-uuid ()
  "return a reasonably-unique uuid"
  (let ((new-uuid (make-v1-uuid)))
    (if (find-user-by-uuid new-uuid)
        (generate-uuid)
        new-uuid)))

(defun find-user-by-username (username)
  "lookup user record by username."
  (with-connection (db)
    (retrieve-one
     (select :*
        (from :user)
       (where (:= :username username)))
     :as 'user)))

 (defun find-user-by-email (email)
   "lookup user record by email."
   (with-connection (db)
     (retrieve-one
       (select :* (from :user)
               (where (:= :email email)))
       :as 'user)))

 (defun find-user-by-uuid (uuid)
  "lookup user record by uuid."
  (with-connection (db) 
    (retrieve-one
      (select :* (from :user)
              (where (:= :uuid uuid)))
      :as 'user)))

(defun find-user (identifier)
   "lookup user record by username or email."
   (or (find-user-by-username identifier)
       (find-user-by-email identifier)
       (find-user-by-uuid identifier)))

(defun authenticate-user (username-or-email password)
  "Lookup user record and validate password. Returns two values:
   1st value, was password correct T or NIL
   2nd value, was user found, T or NIL
   Example:
   (VALUES NIL NIL) -> user not found
   (VALUES NIL T) -> user found, but wrong password
   (VALUES T T) -> password correct"
  (let ((password-hash (getf (find-user username-or-email) :password)))
    (if password-hash
        (values (cl-pass:check-password password password-hash) t)
        (values nil nil))))

;;; TODO: export to db
(defun create-tables ()
  "Create all tables"
  (create-user-table))

(defun create-user-table ()
  "Create user table if it doesn't exist yet."
  (with-connection (db)
    (execute
     (create-table (:user :if-not-exists nil)
         ((id :type 'serial :primary-key t)
          (username :type 'text :not-null t :unique t)
          (email :type 'text :not-null t :unique t)
          (password :type 'text :not-null t)
          (uuid :type 'text :not-null t :unique t))))))
