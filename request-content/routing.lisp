(defpackage #:wst.request-content.routing
  (:use #:cl)
  (:documentation "HTTP routing middleware adapter for wst.request-content.

Provides a WRAP :before middleware constructor that parses request content
using `wst.request-content:parse-content` and stores the parsed value in
`wst.routing:request-data`.")
  (:export
   #:parse-request-content-middleware))

(in-package #:wst.request-content.routing)

(defun %charset->encoding (charset default-encoding)
  (if (and charset (string-equal charset "utf-8"))
      :utf-8
      default-encoding))

(defun parse-request-content-middleware (&key
                                           (request-data-key :content)
                                           (default-content-type "text/plain")
                                           (default-encoding :us-ascii))
  "Create a before-middleware that parses the request body.

Returns a function suitable for `wst.routing.dsl:wrap` :before.

Behavior:
- Reads request content type from `wst.routing:request-content-type`.
- Parses the body with `wst.request-content:parse-content`.
- Stores parsed content in `wst.routing:request-data` under REQUEST-DATA-KEY.
- Returns `(:continue . response)` on success.
- Returns `(:halt . response)` with HTTP 400 on parse errors."
  (lambda (request response)
    (handler-case
        (let* ((content-type (or (wst.routing:request-content-type request)
                                 default-content-type))
               (parsed-type (or (car (wst.request-content:parse-content-type content-type))
                                (cons :|text/plain| nil)))
               (mime (car parsed-type))
               (options (cdr parsed-type))
               (charset (cdr (assoc "charset" options :test #'string=)))
               (encoding (%charset->encoding charset default-encoding))
               (content (wst.request-content:parse-content
                         mime
                         (wst.routing:request-content request)
                         encoding))
               (data (copy-list (wst.routing:request-data request))))
          (setf (getf data request-data-key) content
                (wst.routing:request-data request) data)
          (cons :continue response))
      (error (err)
        (wst.routing:bad-request-response
         t response
         :content (format nil "invalid request content: ~a" err))
        (cons :halt response)))))
