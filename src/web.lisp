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

(defun render-with-session (template &rest args)
  (render template (append args (list :current-user *current-user*))))


;;
;; Routing rules

@route GET "/"
(defun root ()
  (render #P"index.html"))


;;; --- User routes --- ;;;

;;; All users
@route GET "/users"
(defun user-index ()
  (render #P"users/index.html" (list :users (all-users))))

;;;
;;; Show user
@route GET "/u/:uuid"
(defun user-show (&key uuid)
  (try-authenticate-user uuid
    (let ((user (find-user-by-uuid uuid)))
      (render-with-session #P"users/show.html" :user (for-template user)))))


;;; Edit user
@route GET "/u/:uuid/edit"
(defun user-edit (&key uuid)
  "user edit")


;;; Update user
@route POST "/u/:uuid"
(defun user-update (&key uuid)
  )



;;; --- Authentication routes --- ;;;

;;; Auth form
@route GET "/u/:uuid/auth"
(defun user-auth-form (&key uuid)
  (render #P"users/auth.html" (list :uuid uuid)))

;;; Authenticate uuid
@route POST "/u/:uuid/auth"
(defun user-auth (&key uuid _parsed)
  (let ((params (build-params _parsed)))
(print params)
    (print (gethash :password params))
        )
  ;;(let ((password (find 'password _parsed :key 'car))))
  (print _parsed)
  (print (length _parsed))
   ;;(format nil "~S"  (find :uuid _parsed :key 'car))
   (format nil "~S"  (assoc (intern "PASSWORD") _parsed))
  ;; need password from somewhere
  ;;(let ((user (find-user-by-uuid uuid)))
  ;;  (if (and user
  ;;           (authenticate-user user password))
  ;;      (add-user-to-session user)))
  ;;(uuid-show uuid))
  )

(defun build-params (raw)
  "Build a dict from the params cons list."
  (let ((results (make-hash-table)))
    (loop for param_pair in raw
          do (progn
               (print (type-of (car param_pair)))
               (setf (gethash (intern (string (car param_pair))) results)
                     (cdr param_pair))))
    results))


(defun add-user-to-session (user)
  (setf (gethash :current_uuid *session*) (user-uuid user))
  (setf (gethash :ttl *session*) 100))

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
            (progn                          ; clear the session
              (setf (gethash :current_uuid *session*) nil)
              (setf (gethash :ttl *session*) nil))
            (decf (gethash :ttl *session*))))))


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
                (eq ,uuid uuid-from-session))
            ,@body                                   ; render
           (redirect (url-for :user-auth-form))      ; redirect to login
        ))))



;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
