(in-package :cl-user)

(defpackage uuidnet.message
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
  (:export :create-message
           :find-messages-between
           ))

(in-package :uuidnet.message)

(defmodel message
  id
  sender_id
  recipient_id
  reply_id
  body
  created_at
  updated_at)


(defun create-message (&key body sender_id recipient_id reply_id)
  (with-connection (db)
    (execute
     (insert-into :messages
       (set= :sender_id sender_id
             :recipient_id recipient_id
             :reply_id reply_id
             :body body
             :created_at (local-time:now)
             :updated_at (local-time:now))))))


(defun find-messages-by-sender (sender_id)
  (with-connection (db)
    (retrieve-all
     (select :*
        (from :messages)
       (where (:= :sender_id sender_id)))
     :as 'message)))


(defun find-messages-in-thread (message_id)
  (with-connection (db)
    (retrieve-all
     (select :*
        (from :messages)
       (where (:= :reply_id message_id)))
     :as 'message)))


(defun find-messages-by-sender-recipient (sender_id recipient_id)
  (with-connection (db)
    (retrieve-all
     (select :*
        (from :messages)
       (where (:and (:= :sender_id sender_id)
                    (:= :recipient_id recipient_id))))
     :as 'message)))


;; NOTE this is a composite query and could be moved out of this model
(defun find-messages-between (&key sender_id recipient_id)
  "Look up sent and received messages for a given user"
  (list)
  )
