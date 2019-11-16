(in-package :cl-user)
(defpackage uuidnet.web
  (:use :cl
        :caveman2
        :uuidnet.config
        :uuidnet.view
        :uuidnet.db
        :uuidnet.model
        :datafly
        :sxql)
  ;; FIXME: view.render conflicts with a function in web also named render
  (:shadowing-import-from :uuidnet.view
                :render)
  (:shadowing-import-from :uuidnet.model :all-users)
  (:export :*web*))
(in-package :uuidnet.web)

;; for @route annotation
(syntax:use-syntax :annot)

;; Get a value of Referer header.
;;(http-referer *request*)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)
(defvar *current-user* nil)

;;
;; Routing rules

@route GET "/"
(defun root ()
  (render #P"index.html"))


;;; Show uuid
@route GET "/uuid/:uuid"
(defun uuid-show (&key uuid)
  (with-authenticated-user
    (render #P"users/show.html" (list :user (for-template user)))))


;;; Authenticate uuid
@route POST "/uuid/:uuid/auth"
(defun uuid-auth (&key uuid)
  (let ((user (find-user-by-uuid uuid)))
    (if (and user
             (authenticate-user user))
        (add-user-to-session user)))
  (uuid-show uuid))

(defun add-user-to-session (user)
  (setf (gethash :current_uuid *session*) (user-uuid user)))

(defun set-current-user-from-session ()
  (let ((user (find-user-by-uuid
               (gethash :current_uuid *session*))))
    (setf *current-user* user)))

(defmacro with-authenticated-user (&body body)
  `(let ((user (find-user-by-uuid
                (gethash :current_uuid *session*))))
     (if user
         ,@body
         (redirect (url-for :root)))))

;;;
;;; All users
@route GET "/users"
(defun user-index ()
  (render #P"users/index.html" (list :users (all-users))))


;;; New user
(defroute "/users/new" ()
  (render #P"users/new.html"))


;;; Show user
@route GET "/users/:uuid"
(defun user-show (&key uuid)
  (let ((user (find-user-by-uuid uuid)))
    (if user
        (render #P"users/show.html" (list :user (for-template user)))
        (redirect (url-for :root)))))


;;; Edit user
@route GET "/users/:uuid/edit"
(defun user-edit (&key uuid)
  "user edit")


;;; Save user
@route POST "/users/:uuid"
(defun user-update (&key uuid)
    (format "hello there ~A" uuid)
  )


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
