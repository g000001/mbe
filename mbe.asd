;;;; mbe.asd

(cl:in-package :asdf)

(defsystem :mbe
  :serial t
  :components ((:file "package")
               (:file "mbe")))

(defmethod perform ((o test-op) (c (eql (find-system :mbe))))
  (load-system :mbe)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :mbe-internal :mbe))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

