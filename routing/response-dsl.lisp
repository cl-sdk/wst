(defpackage #:io.github.cl-sdk.wst.routing.response.dsl
  (:use #:cl #:io.github.cl-sdk.wst.routing)
  (:export
   #:status
   #:headers
   #:text
   #:html
   #:json))

(in-package :io.github.cl-sdk.wst.routing.response.dsl)

(defun headers (new-headers response)
  "Adds or updates HTTP headers in RESPONSE with NEW-HEADERS.

- NEW-HEADERS: A property list of header key-value pairs to add or update.
- RESPONSE: The response object whose headers are being modified.

Updates RESPONSE by merging NEW-HEADERS into its existing headers, then returns RESPONSE."
  (let ((headers (response-headers response)))
    (loop :for (key value) :on new-headers :by #'cddr
          :do (setf (getf headers key) value))
    (setf (response-headers response) headers)
    response))

(defun status (status response)
  "Sets the HTTP status code of RESPONSE.

- STATUS: An integer representing the HTTP status code to set.
- RESPONSE: The response object to update.

Updates RESPONSE with the given STATUS and returns the modified RESPONSE."
  (setf (response-status response) status)
  response)

(defun set-content-and-headers-for-content-type (content content-type response)
  (headers (list :content-type content-type) response)
  (setf (response-content response)
        content)
  response)

(defun text (content response)
  "Sets CONTENT as plain text in RESPONSE.

- CONTENT: The string data to be included in the response.
- RESPONSE: The response object where the content and appropriate headers are set.

This function sets the Content-Type header to \"text/plain\" and updates RESPONSE with CONTENT."
  (set-content-and-headers-for-content-type
   content
   "text/plain"
   response))

(defgeneric html (implementation content response)
  (:documentation "Converts CONTENT into HTML text using IMPLEMENTATION and sets it in RESPONSE.

- IMPLEMENTATION: The system or method used for the conversion (defaults to T).
- CONTENT: The data to be converted into HTML.
- RESPONSE: The response object where the resulting HTML content and headers are set.

The default method (for IMPLEMENTATION = T) serializes CONTENT as HTML text
and updates RESPONSE accordingly.")
  (:method ((implementation t) content response)
    (set-content-and-headers-for-content-type
     content
     "text/html"
     response)))

(defgeneric json (implementation content response)
  (:documentation "Converts CONTENT into JSON text using IMPLEMENTATION and sets it in RESPONSE.

- IMPLEMENTATION: The system or method used for the conversion (defaults to T).
- CONTENT: The data to be converted into JSON.
- RESPONSE: The response object where the resulting JSON content and headers are set.

The default method (for IMPLEMENTATION = T) serializes CONTENT as JSON text
and updates RESPONSE accordingly.")
  (:method ((implementation t) content response)
    (set-content-and-headers-for-content-type
     content
     "application/json"
     response)))
