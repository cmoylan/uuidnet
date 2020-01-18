(in-package :cl-user)

(defpackage uuidnet.user
  (:use :cl :sxql :uuid)
  (:import-from :uuidnet.db
                :db
                :with-connection
                :with-transaction)
  (:import-from :datafly
                :defmodel
                :execute
                :retrieve-all
                :retrieve-one)
  (:import-from :uuidnet.utilities.nickname
                :make-nickname)
  (:nicknames :user)
  (:export :all-users
           :authenticate-user
           :find-user
           :find-user-by-uuid
           :find-user-by-public-identifier
           :user-for-template
           :add-user
           :add-open-user
           :seed-users
           :user-requires-auth-p
           :user-uuid
           :password-set-p
           :user-id))

(in-package :uuidnet.user)

(defmodel user
  id
  username
  uuid
  email
  password
  created-at
  updated-at)


(defun seed-users ()
  "Create some test users"
  (add-user "cmoylan@example.com" "abc123")
  )


(defun user-for-template (user)
  "transform the user struct into an alist"
  (list :email (user-email user)
    :uuid (user-uuid user)
    :username (user-username user)
    :password-set (password-set-p user)))


(defun password-set-p (user)
  "return a boolean indicating if the user has a password"
  (not (null (user-password user))))


(defun all-users ()
  "return all users."
  (with-connection (db)
    (retrieve-all
     (select :*
       (from :users))
     :as 'user)))


(defun add-user (email password)
  "add user record to database"
  (with-connection (db)
    (execute
     (insert-into :users
       (set= :email email
             :password (cl-pass:hash password)
             :uuid (generate-uuid)
             :username (generate-username)
             :created_at (local-time:now)
             :updated_at (local-time:now))))))


(defun add-open-user ()
  "Add a user without a password"
  (let ((uuid (generate-uuid))
        (username (generate-username)))
    (with-connection (db)
      (execute
       (insert-into :users
         (set= :uuid uuid
               :username username
               :created_at (local-time:now)
               :updated_at (local-time:now)))))
    (find-user-by-uuid uuid)))


(defun generate-uuid ()
  "return a reasonably-unique uuid"
  (let ((new-uuid (make-v1-uuid)))
    (if (find-user-by-uuid new-uuid)
        (generate-uuid)
        new-uuid)))


;; TODO should raise an error if this tries 10 times without success
(defun generate-username ()
  "return a unique username"
  (let ((new-username (make-nickname)))
    (if (find-user-by-username new-username)
        (generate-username)
        new-username)))


(defun find-user-by-email (email)
  "lookup user record by email"
  (with-connection (db)
    (retrieve-one
      (select :* (from :users)
              (where (:= :email email)))
      :as 'user)))


(defun find-user-by-username (username)
  "lookup user record by username"
  (with-connection (db)
    (retrieve-one
     (select :*
        (from :users)
       (where (:= :username username)))
     :as 'user)))


 (defun find-user-by-uuid (uuid)
  "lookup user record by uuid"
  (if uuid
    (with-connection (db)
      (retrieve-one
       (select :* (from :users)
               (where (:= :uuid uuid)))
       :as 'user))))


(defun find-user-by-public-identifier (identifier)
  "lookup user by a public identifier - username or uuid"
  (or (find-user-by-username identifier)
      (find-user-by-uuid identifier)))


(defun find-user (identifier)
   "lookup user record by username, email, or uuid"
   (or (find-user-by-username identifier)
       (find-user-by-email identifier)
       (find-user-by-uuid identifier)))


(defun user-requires-auth-p (user)
  (and user
       (not (null (user-password user)))))


(defun authenticate-user (user password)
  "Lookup user record and validate password. Returns two values:
   1st value, was password correct T or NIL
   2nd value, was user found, T or NIL
   Example:
   (VALUES NIL NIL) -> user not found
   (VALUES NIL T) -> user found, but wrong password
   (VALUES T T) -> password correct"
  (let ((password-hash (user-password user)))
    (if password-hash
        (values (cl-pass:check-password password password-hash) t)
        (values nil nil))))
