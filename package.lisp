;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :mbe
  (:export :define-syntax
           :letrec-syntax
           :let-syntax
           ; :*** cl:***
           ))

(defpackage :mbe-internal
  (:use :mbe :cl :fiveam))

