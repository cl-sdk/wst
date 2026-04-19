(defpackage #:wst.request-content.routing
  (:use #:cl)
  (:documentation "HTTP routing middleware adapter for wst.request-content.

Provides a WRAP :before middleware constructor that parses request content
using `wst.request-content:parse-content` and stores the parsed value in
`wst.routing:request-content`.")
  (:export
   #:parse-request-content))

(in-package #:wst.request-content.routing)

(defun %charset->encoding (charset default-encoding)
  (cond
    ((null charset) default-encoding)
    ((string-equal charset "utf-8") :utf-8)
    ((or (string-equal charset "us-ascii")
         (string-equal charset "ascii"))
     :us-ascii)
    ((string-equal charset "iso-8859-1") :latin-1)
    (t default-encoding)))

(defun %default-parsed-type (content-type default-content-type)
  (let ((fallback (or (car (wst.request-content:parse-content-type default-content-type))
                      (cons :|text/plain| nil))))
    (if (or (null content-type)
            (string= (string-trim '(#\Space #\Tab) content-type) ""))
        fallback
        (or (car (wst.request-content:parse-content-type content-type))
            fallback))))

(defun parse-request-content (&key
                                (default-content-type "text/plain")
                                (default-encoding :us-ascii))
  "Create a before-middleware that parses the request body.

Returns a function suitable for `wst.routing.dsl:wrap` :before.

Behavior:
- Reads request content type from `wst.routing:request-content-type`.
- Parses the body with `wst.request-content:parse-content`.
- Stores parsed content in `wst.routing:request-content`.
- Returns `(:continue . response)` on success.
- Returns `(:halt . response)` with HTTP 400 on parse errors."
  (check-type default-content-type string)
  (lambda (request response)
    (handler-case
        (let* ((content-type (wst.routing:request-content-type request))
               (parsed-type (%default-parsed-type content-type default-content-type))
               (mime (car parsed-type))
               (options (cdr parsed-type))
               (charset (cdr (assoc "charset" options :test #'string=)))
               (encoding (%charset->encoding charset default-encoding))
               (content (wst.request-content:parse-content mime
                                                           (wst.routing:request-content request)
                                                           encoding)))
          (setf (wst.routing:request-content request) content)
          (cons :continue response))
      (error (err)
        (wst.routing:bad-request-response
         t response
         :content (format nil "invalid request content: ~a" err))
        (cons :halt response)))))
