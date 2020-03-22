(defsystem "uuidnet"
  :version "0.1.0"
  :author "crushdepth"
  :license ""
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
               "datafly"
               "sxql"

               ;; hashing password
               "cl-pass"

               ;; generate unique identifiers
               "uuid"

               ;; make stuct and class access easier
               ;; "access"

               ;; iterate more easily, avoid loop
               ;; "iterate"

               ;; manipulate time
               "local-time"

               )
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "user" "message" "presenters"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "model" :depends-on ("db")) ;; model
                 (:file "config")
                 (:file "schema" :depends-on ("db"))
                 (:file "message" :depends-on ("db"))
                 (:file "user" :depends-on ("db" "utilities"))
                 (:file "presenters" :depends-on ("db" "user" "message" "convenience"))
                 (:file "convenience")
                 (:module "utilities"
                          :components
                          ((:file "nickname")))
                 )))



  :description ""
  :in-order-to ((test-op (test-op "uuidnet-test"))))
