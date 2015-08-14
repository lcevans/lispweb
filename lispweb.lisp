;;;; lispweb.lisp

;; Note: This borrows heavily from the tutorial at http://www.adamtornhill.com/articles/lispweb.htm

(in-package #:lispweb)


(defclass post ()
  ((title :reader title
          :initarg :title)
   (body :reader body
         :initarg :body)
   (votes :accessor votes
          :initarg :votes)))

(defun upvote-post (post) (incf (votes post)))
(defun downvote-post (post) (decf (votes post)))

(defvar post1 (make-instance 'post :title "Why aren't we living in underwater cities?" :body "20,000 Leagues under the Sea was written 150 years ago!" :votes 12))
(defvar post2 (make-instance 'post :title "Improve productivity by standing on your head." :body "Studies show key is more blood and oxygen to the brain." :votes 4))
(defvar post3 (make-instance 'post :title "Glrky Sold for $3B." :body "The market for automated cutlery is hot right now!" :votes -1))

(defvar *post-list* (list post1 post2 post3))

(defun posts ()
   (sort (copy-list *post-list*) #'> :key #'votes))

(defun post-from-title (titl)
 (find titl *post-list*
       :test #'string-equal
       :key #'title))

(defun add-post (post)
  (push post *post-list*))


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


(defmacro define-url-fn ((name) &body body)
   `(progn
          (defun ,name ()
                  ,@body)
              (push (create-prefix-dispatcher ,(format nil "/~(~a~).htm" name) ',name) *dispatch-table*)))



(define-url-fn (roller-news)
   (standard-page (:title "Roller News")
     (:p (:a :href "new-post.html" "Submit New Post"))
     (:div :class "rn-background"
     (:ul
      (dolist (post (posts))
        (htm
         (:li (fmt "(~d votes)" (votes post))
              (:a :href (format nil "display-post.html?title=~a" (title post)) (fmt "~A" (title post)))
              )
         ))))
     ))

(define-url-fn (upvote)
  (let ((post (post-from-title (parameter "title"))))
    (if post
        (upvote-post post))
    (redirect "/roller-news.htm")))

(define-url-fn (downvote)
  (let ((post (post-from-title (parameter "title"))))
    (if post
        (downvote-post post))
    (redirect "/roller-news.htm")))

(define-url-fn (display-post)
  (let ((post (post-from-title (parameter "title"))))
   (standard-page (:title "Roller News")
     (:h1 (fmt "~A" (title post)))
     (:a :href (format nil "upvote.html?title=~a" (title post)) "Upvote")
     (:a :href (format nil "downvote.html?title=~a" (title post)) "Downvote")
     (:p (fmt "~a" (body post))))))

(define-url-fn (new-post)
  (standard-page (:title "Roller News")
  (:h1 "Submit a new post")
  (:form :action "/create-post.htm" :method "post"
         (:p "Title: "
             (:input :type "text"
                     :name "title"
                     :class "txt"))
         (:p "Body: "
             (:input :type "text"
                     :name "body"
                     :class "txt"))
         (:p (:input :type "submit"
                     :value "Submit"
                     :class "btn")))))

(define-url-fn (create-post)
  (let ((title (parameter "title")) (body (parameter "body")))
    (add-post (make-instance 'post :title title :body body :votes 0))
    (redirect "/roller-news.htm")))


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
