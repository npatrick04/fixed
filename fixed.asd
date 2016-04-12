;;;; fixed.asd

(asdf:defsystem #:fixed
  :description "A fixed-point number type."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "fixed")))

(asdf:defsystem #:fixed/real-time
  :description "A fixed-point internal real-time type."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:fixed)
  :components ((:file "time")))

