(defpackage #:wst.routing.response.dsl
  (:use #:cl)
  (:export
   #:status
   #:headers
   #:text
   #:html
   #:json))

(in-package :wst.routing.response.dsl)

(defun headers (new-headers response)
  (let ((headers (wst.routing:response-headers response)))
    (loop :for (key value) :on new-headers :by #'cddr
          :do (setf (getf headers key) value))
    (setf (wst.routing:response-headers response) headers)
    response))

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

(defgeneric html (implementation content response)
  (:documentation "Using IMPLEMENTATION to convert CONTENT
into html text and put it in the RESPONSE.

Default t is html as text.")
  (:method ((implementation t) content response)
    (set-content-and-headers-for-content-type
     content
     "text/html"
     response)))

(defgeneric json (implementation content response)
  (:documentation "Using IMPLEMENTATION to convert CONTENT
into json text and put it in the RESPONSE.

Default t is json as text.")
  (:method ((implementation t) content response)
    (set-content-and-headers-for-content-type
     content
     "application/json"
     response)))
