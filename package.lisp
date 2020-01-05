;;;; package.lisp

(cl:in-package cl-user)

(defpackage "https://github.com/g000001/mbe"
  (:export define-syntax
           syntax-rules
           letrec-syntax
           let-syntax
           with
           ;; cl:***
           ))

(defpackage "https://github.com/g000001/mbe#internals"
  (:use "https://github.com/g000001/mbe" cl))
