;;;; lispweb.lisp

(in-package #:lispweb)

;;; "lispweb" goes here. Hacks and glory await!

(write-line "Loaded lispweb")

(defun start-web-server
  (port)
  "Starts web server"
  (write-line "Starting web server...")
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))
