;;;; lispweb.asd

(asdf:defsystem #:lispweb
  :description "Describe lispweb here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot)
  :serial t
  :components ((:file "package")
               (:file "lispweb")))

