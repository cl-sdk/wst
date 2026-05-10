(defpackage #:io.github.cl-sdk.wst.routing.woo
  (:use #:cl)
  (:export
   #:request-from-woo-env
   #:response-to-woo-response))

(in-package :io.github.cl-sdk.wst.routing.woo)

(defun request-from-woo-env (env)
  "Constructs a request object from WOO web server environment ENV.

Arguments:
  - ENV: A property list representing the HTTP request environment provided by WOO.

Returns:
  A REQUEST structure populated with data extracted and parsed from ENV:
    - URI, QUERY, and HASH components parsed from :request-uri.
    - Headers from :headers.
    - HTTP method from :request-method.
    - Content-Type and Content-Length headers.
    - Raw request body from :raw-body.
    - Additional data containing the original ENV for reference.

This function translates the raw environment data from the WOO server into a
standardized request object suitable for further processing."

  (multiple-value-bind (path query hash)
      (io.github.cl-sdk.wst.routing:parse-uri (getf env :request-uri))
    (io.github.cl-sdk.wst.routing:make-request :uri path
                                               :query query
                                               :hash hash
                                               :headers (getf env :headers)
                                               :method (getf env :request-method)
                                               :content-type (getf env :content-type)
                                               :content-length (or (getf env :content-length) 0)
                                               :content (getf env :raw-body)
                                               :data (list :env env))))

(defun response-to-woo-response (response)
  "Converts a RESPONSE object into the format expected by the WOO web server.

Arguments:
  - RESPONSE: The response object containing status, headers, and content.

Returns:
  A list of three elements:
    1. The HTTP status code from RESPONSE.
    2. The HTTP headers from RESPONSE.
    3. A list containing the response content string.

This function adapts the internal RESPONSE structure to the WOO server’s response
format for sending back to the client."
  (list (io.github.cl-sdk.wst.routing:response-status response)
        (io.github.cl-sdk.wst.routing:response-headers response)
        (list (io.github.cl-sdk.wst.routing:response-content response))))
