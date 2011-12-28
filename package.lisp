;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :mbe
  (:export :define-syntax
           :syntax-rules
           :letrec-syntax
           :let-syntax
           :with
           ; :*** cl:***
           ))

(defpackage :mbe-internal
  (:use :mbe :cl :fiveam))

