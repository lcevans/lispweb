;;;; lispweb.asd

(asdf:defsystem #:lispweb
  :description "A simple Webapp in LISP"
  :author "Chris Evans"
  :license "TBD"
  :depends-on (#:hunchentoot #:cl-who)
  :serial t
  :components ((:file "package")
               (:file "lispweb")))
