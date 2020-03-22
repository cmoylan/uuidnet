(in-package :cl-user)
(defpackage uuidnet.presenters
  (:use :cl :sxql)
  (:import-from :uuidnet.db
                :db
                :with-connection
                :with-transaction)

  (:import-from :datafly
                :execute
                :retrieve-all
                :retrieve-one)
  (:import-from :uuidnet.user
                :user)
  (:import-from :uuidnet.message
                :message
                :message-for-template)
  (:import-from :uuidnet.convenience
                :append-list)

  (:export :messages-with-senders-by-recipient
           :messages-with-users-between
           :group-messages-by-sender))

(in-package :uuidnet.presenters)


(defstruct (message-with-sender (:include message))
  sender-username
  recipient-username
  )


(defun messages-with-senders-by-recipient (recipient_id)
  "return messages for a recipient with nested sender user objects"
  (with-connection (db)
    (retrieve-all
     (select (:messages.*
              (:as :sender.username :sender-username))
      (from :messages)
      (inner-join (:as :users :sender) :on (:= :sender.id :messages.sender_id))
      (where (:= :recipient_id recipient_id))
      ;;(group-by :messages.id :sender.username)
      (order-by :sender_id (:asc :created_at))
      )
     :as 'message-with-sender)))


(defun messages-with-users-between (&key sender_id recipient_id)
  "return messages between two users with nested user objects"
  (with-connection (db)
    (retrieve-all
     (select (:messages.*
              (:as :sender.username :sender-username)
              (:as :recipient.username :recipient-username))
      (from :messages)
      (inner-join (:as :users :sender) :on (:= :sender.id :messages.sender_id))
      (inner-join (:as :users :recipient) :on (:= :recipient.id :messages.recipient_id))
      (where (:and (:= :sender_id sender_id)
                   (:= :recipient_id recipient_id))))
     :as 'message-with-sender)))



(defun group-messages-by-sender (messages)
  "return an alist of messages grouped by sender"
  (let ((results (list))
        (current-sender)
        (current-messages))
    (loop for message in messages do
      (progn
        (if (string= (message-with-sender-sender-username message) current-sender)
            ;;; true: still building the current-group
            (setf current-messages (append-list current-messages
                                                (append (message-for-template message)
                                                        (list :sender-username (message-with-sender-sender-username message)
                                                              :recipient-username (message-with-sender-recipient-username message)))))
            ;;; false: start a new group
            (progn
              ;; write the queued up messages to results
              (if (and current-sender current-messages)
                  (setf results (append-list results
                                             (list :sender-username current-sender
                                                   :messages current-messages))))


              ;; start building a new current-group
              (setf current-sender (message-with-sender-sender-username message))
              (setf current-messages (list (message-for-template message)))))))

    ;; add the last group to the results
    (setf results (append-list results
                               (list :sender-username current-sender
                                     :messages current-messages)))
    results))
