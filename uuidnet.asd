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
               )
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "model"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "model" :depends-on ("db")) ;; model
                 (:file "config")
                 (:file "schema" :depends-on ("db"))


                 (:module "models"
                  :components
                  ((:file "message"))
                  )
                 )))

               ;;;(:module "db"
               ;;;         :pathname "db"
               ;;;         :components
               ;;;         ((:file "schema" :depends-on ("src")))))
  :description ""
  :in-order-to ((test-op (test-op "uuidnet-test"))))
