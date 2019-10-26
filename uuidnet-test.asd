(defsystem "uuidnet-test"
  :defsystem-depends-on ("prove-asdf")
  :author "crushdepth"
  :license ""
  :depends-on ("uuidnet"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "uuidnet"))))
  :description "Test system for uuidnet"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
