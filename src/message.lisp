(in-package :cl-user)

(defpackage uuidnet.message
  (:use :cl :sxql :local-time)
  (:import-from :uuidnet.db
                :db
                :with-connection
                :with-transaction)
  (:import-from :datafly
                :defmodel
                :execute
                :retrieve-all
                :retrieve-one)
  ;(:import-from :local-time
  ;              : )
  (:nicknames :message)
  (:export :create-message
           :create-threaded-message
           :find-messages-between
           :find-messages-by-recipient
           :message-for-template
           :message-sender
           :message-sender-username
           ))

(in-package :uuidnet.message)


;;;(defmodel (user (:inflate registered-at #'datetime-to-timestamp)
;;;                (:has-a config (where (:= :user_id id)))
;;;                (:has-many (tweets tweet)
;;;                 (select :*
;;;                   (from :tweet)
;;;                   (where (:= :user_id id))
;;;                   (order-by (:desc :created_at)))))


;; TODO make some utility function to fetch a message and join it's users,
;;      but make it efficient
(defmodel message
  id
  sender-id
  recipient-id
  reply-id
  body
  created-at
  updated-at)


;; TODO need to use conversation_id or something so i can grab the replies (where sender_id is current_user)
;;      conversation_id will need to be a primary key
;;      we'll also probably was the ability to look up a conversation by 2 uuids/usernames
;;
;;      look in the database for an existing message between uuid and current_uuid
;;      if one is found use that
;;      if one is not found, iterate the last...or set up the colum properly so this happens automatically
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


(defun create-threaded-message (&key body sender_id recipient_id)
  "Just like create-message but it will set the reply_id automatically"
  ; reply-id
  (let ((last-id 0)
        (last-message
          (last-message-between :sender_id sender_id :recipient_id recipient_id)))
    (if last-message
        (setf last-id (message-id last-message)))

    (create-message :body body
                    :sender_id sender_id
                    :recipient_id recipient_id
                    :reply_id last-id)
    )

 )

(defun find-messages-by-sender (sender_id)
  (with-connection (db)
    (retrieve-all
     (select :*
      (from :messages)
       (where (:= :sender_id sender_id)))
     :as 'message)))


(defun find-messages-by-recipient (recipient_id)
  ;;; TODO - order by date
  ;;;      - group by sender
  (with-connection (db)
    (retrieve-all
     (select :*
      (from :messages)
      (inner-join (:as :users :sender) :on (:= :sender.id :messages.sender_id))
      (where (:= :recipient_id recipient_id))
      ;(group-by :sender_id)
      )
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


(defun last-message-between (&key sender_id recipient_id)
  "only return the latest message between two users"
  (with-connection (db)
    (retrieve-one
     (select :*
      (from :messages)
       (where (:and (:= :sender_id sender_id)
                    (:= :recipient_id recipient_id)))
       (limit 1)
       (order-by (:desc :created_at)))
     :as 'message)))


(defun message-for-template (message)
  "transform the messages struct into an alist"
  (list :sender_id (message-sender-id message)
        :recipient_id (message-recipient-id message)
        :reply_id (message-reply-id message)
        :body (message-body message)
        :created_at (local-time:format-timestring nil
                     (local-time:universal-to-timestamp (message-created-at message))
                     :format (list :short-month " " :day ", " :year " @ " :hour ":" :min ))
        ;:sender (message-sender message)
        ))
