(in-package :cl-user)
(defpackage uuidnet.web
  (:use :cl
        :caveman2
        :alexandria
        :uuidnet.config
        :uuidnet.view
        :uuidnet.db
        :uuidnet.user
        :uuidnet.message
        :datafly
        :sxql)
  (:export :*web*))
(in-package :uuidnet.web)


;; for @route annotation
(syntax:use-syntax :annot)


;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)
(defvar *current-user* nil)
(defvar *flash* nil)


(defun render-with-session (template &rest args)
  "Add flash and current-user to the template variables"
  (let ((session-params (list :current-user
                              (if *current-user*
                                  (user-for-template *current-user*)))))
    (if *flash*
        (progn
          (setf session-params (append session-params
                                       (list :flash-type (string-downcase (first *flash*))
                                             :flash-message (second *flash*))))
          (setf *flash* nil)))
    (view:render template (append args session-params))))


(defmacro with-flash (type message &body body)
  "Set a flash message"
  `(progn
     (setf *flash* (list ,type ,message))
     ,@body))


;;; Authentication
;;;
(defun add-user-to-session (user)
  (setf (gethash :current_uuid *session*) (uuidnet.user:user-uuid user))
  (setf (gethash :ttl *session*) 100))


(defun clear-session ()
  (setf (gethash :current_uuid *session*) nil)
  (setf (gethash :ttl *session*) nil)
  (setf *current-user* nil))


(defun set-current-user-from-session ()
  (let ((user (find-user-by-uuid
               (gethash :current_uuid *session*))))
    (setf *current-user* user)))


(defun try-expire-session ()
  "If there is a user in the session decrememt the ttl.
   If the ttl is below 0 expire the session."
  (let ((ttl (gethash :ttl *session*)))
    (if ttl
        (if (< ttl 0)
            (clear-session)
            (decf (gethash :ttl *session*))))))


(defun build-params (raw)
  "Build a dict from the params cons list."
  (let ((results (make-hash-table)))
    (loop for param_pair in raw
          do (progn
               (setf (gethash (intern
                               (string-upcase
                                (string (car param_pair)))) results)
                     (cdr param_pair))))
    results))


(defmacro try-authenticate-user (uuid &body body)
  "Check session user against current action"
  `(progn
     (try-expire-session)
     ;; page is open
     ;; - render page
     ;; page is closed
     ;; - if session user is page user
     ;; -- render page
     ;; - if seesion user is not page user
     ;; -- redirect
     (let ((user-for-page (find-user-by-uuid ,uuid))
            (uuid-from-session (gethash :current_uuid *session* nil)))
        (if (or (not (user-requires-auth-p user-for-page))
                (string= ,uuid uuid-from-session))
            (progn
              (set-current-user-from-session)
              ,@body)                                   ; render
           (redirect (url-for :user-auth-form  :uuid ,uuid)))))) ; redirect to login

(defun require-user ()
  (if (not *current-user*)
      (let ((referer (gethash "referer" (request-headers *request*))))
        (with-flash :error "Please sign in"
          (redirect (or referer (url-for :root)))))))


@route GET "/referer"
(defun refer-test ()
  (gethash "referer" (request-headers *request*)))


@route GET "/login-test"
(defun login-test ()
  (require-user)
  "you are signed in")


;;
;; Routing rules

@route GET "/"
(defun root ()
  (render-with-session #P"index.html"))


;;; --- User routes --- ;;;

;;; All users
@route GET "/users"
(defun user-index ()
  (render-with-session #P"users/index.html" :user-uuids
                                     (map 'list
                                          #'user-uuid
                                          (user:all-users))))


;;; Show user
@route GET "/u/:uuid"
(defun user-show (&key uuid)
  (try-authenticate-user uuid
    (let ((user (find-user-by-uuid uuid)))
          ;(messages (find-messages-for-uuid uuid)))
      (render-with-session #P"users/show.html" :user (user-for-template user)))))


;;; Edit user
@route GET "/u/:uuid/edit"
(defun user-edit (&key uuid)
  "user edit")


;;; Update user
@route POST "/u/:uuid"
(defun user-update (&key uuid)
  )


@route POST "/users"
(defun user-create ()
  "Create an open user."
  (let ((new_user (add-open-user)))
  (redirect (url-for :user-show :uuid (user-uuid new_user)))))


;;; --- Message routes --- ;;;

;;; Create message
;; TODO need to handle if create-message fails
@route POST "/u/:uuid/messages"
(defun message-create (&key uuid _parsed)
  (require-user)
  (let ((params (build-params _parsed))
        (recipient (find-user-by-uuid uuid)))
    (create-message :body (gethash (intern "MESSAGE-BODY") params)
                    :sender_id (user-id *current-user*)
                    :recipient_id (user-id recipient)
                    :reply_id nil)
    (redirect (url-for :user-show :uuid uuid))))


@route GET "/u/:uuid/messages"
(defun message-show (&key uuid)
  "Show messages between the uuid and current-user"
  (require-user)
  (let* ((user (find-user-by-uuid uuid))
         (messages (find-messages-between :sender_id (user-id *current-user*)
                                          :recipient_id (user-id user))))
    (render-with-session #P"messages/show.html" :messages (map 'list #'message-for-template messages))))
    ;(render-with-session #P"messages/show.html" :messages (list messages))))


;;; --- Authentication routes --- ;;;

@route GET "/u/:uuid/auth"
(defun user-auth-form (&key uuid)
  "Show the auth form for a user"
  (view:render #P"users/auth.html" (list :uuid uuid)))


@route POST "/u/:uuid/auth"
(defun user-auth (&key uuid _parsed)
  "Handle the submission of the auth form"
  (let* ((params (build-params _parsed))
         (password (gethash (intern "PASSWORD") params))
         (user (find-user-by-uuid uuid)))
    (if (and user
             (authenticate-user user password))
        (progn
          (print "user is authenticated")
          (add-user-to-session user)
          (redirect (url-for :user-show :uuid uuid)))
        (progn
          (print "wrong password!")
          (with-flash :error "incorrect passowrd"
            (redirect (url-for :user-auth-form :uuid uuid)))))))


@route POST "/u/:uuid/login"
(defun user-login (&key uuid)
  "Login as an open user"
  (let ((user (user:find-user-by-uuid uuid)))
    (if (not (user:password-set-p user))
        (progn (add-user-to-session user)
               (redirect (url-for :user-show :uuid uuid)))
        (redirect (url-for :user-auth-form :uuid uuid)))))


@route GET "/logout"
(defun user-logout ()
  (clear-session)
  (redirect (url-for :root)))



;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
