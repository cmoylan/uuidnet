(in-package :cl-user)
(defpackage uuidnet.convenience
  (:use :cl)

  (:export :append-list))

(in-package :uuidnet.convenience)

(defun append-list (original new)
  (append original (list new)))
