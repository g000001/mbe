;;;; mbe.asd

(cl:in-package :asdf)

(defsystem :mbe
  :version "20200106"
  :description "Scheme Macros for Common Lisp"
  :long-description "Scheme Macros for Common Lisp (Unhygenic)
http://www.ccs.neu.edu/~dorai/mbe/mbe-lsp.html"
  :author "Dorai Sitaram"
  :maintainer "CHIBA Masaomi"
  :license "LGPL 2.1"
  :serial t
  :components ((:file "package")
               (:file "mbe")))

(defmethod perform :after ((o load-op) (c (eql (find-system :mbe))))
  (let ((name "https://github.com/g000001/mbe")
        (nickname :mbe))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))
