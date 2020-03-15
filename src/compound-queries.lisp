(in-package :cl-user)
(defpackage uuidnet.compound-queries
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
                :message)

  (:export :messages-with-senders-by-recipient))
(in-package :uuidnet.compound-queries)


(defstruct (message-with-sender (:include message))
  sender-username
  )

(defun messages-with-senders-by-recipient (recipient_id)
  "return messages for a recipient with nested sender user objects"
  (with-connection (db)
    (retrieve-all
     (select (:messages.*
              (:as :sender.username :sender-username)
              )
      (from :messages)
      (inner-join (:as :users :sender) :on (:= :sender.id :messages.sender_id))
      (where (:= :recipient_id recipient_id))
      (group-by :messages.id :sender.username)
      )
     :as 'message-with-sender)))
