(defpackage #:wst.routing.response.dsl
  (:use #:cl)
  (:export
   #:status
   #:headers
   #:text
   #:html
   #:json))

(in-package :wst.routing.response.dsl)

(defun headers (headers response)
  (setf (wst.routing:response-headers response)
        (append (wst.routing:response-headers response) headers))
  response)

(defun status (status response)
  (setf (wst.routing:response-status response) status)
  response)

(defun set-content-and-headers-for-content-type (content content-type response)
  (headers (list :content-type content-type) response)
  (setf (wst.routing:response-content response)
        content)
  response)

(defun text (content response)
  (set-content-and-headers-for-content-type
   content
   "text/plain"
   response))

(defun html (content response)
  (set-content-and-headers-for-content-type
   content
   "text/html"
   response))

(defun json (content response)
  (set-content-and-headers-for-content-type
   content
   "application/json"
   response))
