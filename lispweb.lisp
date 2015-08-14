;;;; lispweb.lisp

(in-package #:lispweb)


(defun question (title body)
  (list :title title :body body))

(defun get-title (question) (write-string (getf question :title)))
(defun get-body (question) (write-string (getf question :body)))

(defvar *question-list* (list (question "Building a mousetrap with a raspberry pi." "Click me") (question "Glrty raises $2B." "Click me")))

(defun question-to-list-elem (question)
  (write-string
   (with-html-output-to-string (*standard-output* nil :indent t)
                               (:li
                                (:b (get-title question))
                                (get-body question)))))


(defun questions-to-list (questions)
  (with-html-output-to-string (*standard-output* nil :indent t)
                              (:ul
                               (mapcar #'question-to-list-elem questions))))

(write-line (questions-to-list *question-list*))

(defmacro standard-page ((&key title) &body body)
   `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
                                   (:html :xmlns "http://www.w3.org/1999/xhtml"
                                            :xml\:lang "en"
                                              :lang "en"
                                                (:head
                                                    (:meta :http-equiv "Content-Type"
                                                             :content  "text/html;charset=utf-8")
                                                       (:title ,title)
                                                          (:link :type "text/css"
                                                                   :rel "stylesheet"
                                                                     :href "/stylesheet.css"))
                                                  (:body
                                                      (:div :id "header" ; Roller News header
                                                             (:img :src "/logo.png"
                                                                          :alt "Roller News logo"
                                                                                 :class "logo")
                                                              (:span :class "strapline"
                                                                     "Roller News: All the news that's fit to roll."))
                                                         ,@body))))



(define-easy-handler (test-page :uri "/"
                                :default-request-type :get)
    ((state-variable :parameter-type 'string))
    (standard-page (:title "Roller News") (write-string (questions-to-list *question-list*))))

;; This is ghetto. TODO: Proper lookup of static files
(push (create-static-file-dispatcher-and-handler
       "/stylesheet.css" "static/stylesheet.css")
      *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
       "/logo.png" "static/logo.png")
      *dispatch-table*)


(defun start-web-server
  (port)
  "Starts web server"
  (write-line "Starting web server...")
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :port port
                                    :document-root #p"static")))
                                    ;; :document-root (merge-pathname #p"static" (sb-posix:getcwd)))))
