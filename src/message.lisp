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
  (:nicknames :message)
  (:export :create-message
           :find-messages-between
           :message-for-template
           ))

(in-package :uuidnet.message)

(defmodel message
  id
  sender-id
  recipient-id
  reply-id
  body
  created-at
  updated-at)


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


(defun find-messages-between (&key sender_id recipient_id)
  "look up messages between two users"
  (with-connection (db)
    (retrieve-all
     (select :*
      (from :messages)
       (where (:and (:= :sender_id sender_id)
                    (:= :recipient_id recipient_id))))
     :as 'message)))


(defun message-for-template (message)
  "transform the messages struct into an alist"
  (print "---------")
  (print (message-created-at message))
  (list :sender_id (message-sender-id message)
    :recipient_id (message-recipient-id message)
    :reply_id (message-reply-id message)
    :body (message-body message)
    :created_at (message-created-at message)))
