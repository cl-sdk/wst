(in-package :io.github.cl-sdk.wst.routing)

(defun write-response (response
                       status
                       headers
                       content
                       &key
                         (content-type "text/html"))
  "Write to RESPONSE, optionally, the CONTENT, STATUS, HEADERS
and CONTENT-TYPE (default is text/html)."
  (setf (response-status response) status
        (response-headers response) (append (response-headers response)
                                            (list :content-type content-type)
                                            headers)
        (response-content response) content)
  response)

(defgeneric created-response (ty response &key headers content)
  (:documentation "Constructs a 201 Created HTTP response based on the specified content type.

- TY: A keyword indicating the response format. (T defaults to HTML).
- RESPONSE: The response object to be serialized and sent.
- :HEADERS (optional): Additional HTTP headers to include in the response.
- :CONTENT (optional): The body of the response. Usually empty, but can be defined.
                       It will be serialized according to the specified type.

Dispatches on TY to format and serialize the RESPONSE appropriately. The default
method (for TY = T) sends an empty content body with status 201 and any provided headers.")
  (:method ((ty t) response &key headers content)
    (write-response response 201 headers (or content ""))))

(defgeneric ok-response (ty response &key headers content)
  (:documentation "Constructs an HTTP 200 OK response based on the specified content type.

- TY: A keyword indicating the desired response format. (T defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.
- :HEADERS (optional): A plist of additional HTTP headers to include in the response.
- :CONTENT (optional): The body of the response. It will be serialized according to
                       the specified type. Default is empty response.

Dispatches on the TY argument to format and serialize the response appropriately.
Custom methods should implement content handling for each supported type. The
default method (for TY = T) returns the provided content with a 200 status and
any specified headers.")
  (:method ((ty t) response &key headers content)
    (write-response response 200 headers (or content ""))))

(defgeneric internal-server-error-response (ty response &key headers content)
  (:documentation "Constructs an HTTP 500 Internal Server Error response based on the specified content type.

- TY: A keyword indicating the desired response format. Supported values include
      :json, :html, and T (which defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.
- :HEADERS (optional): A plist of additional HTTP headers to include in the response.
- :CONTENT (optional): The body of the response. It will be serialized according to
                       the specified type.

Dispatches on the TY argument to format and serialize the response appropriately.
Custom methods should implement content handling for each supported type. The
default method (for TY = T) returns the provided content with a 500 status and
any specified headers. If no content is provided, a fallback response is generated
using DEFAULT-INTERNAL-SERVER-ERROR-RESOUNSE."
                  )
  (:method ((ty t) response &key headers content)
    (write-response response 500 headers (or content "Internal server error"))))

(defgeneric not-found-response (ty response &key headers content)
  (:documentation "Constructs an HTTP 404 Not Found response based on the specified content type.

- TY: A keyword indicating the desired response format. (T defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.
- :CONTENT (optional): The body of the response. It will be serialized according to
                       the specified type. Defaults to \"not found\" if not provided.

Dispatches on the TY argument to format and serialize the response appropriately.
Custom methods should implement content handling for each supported type. The
default method (for TY = T) returns the given content (or a default message) with
a 404 status.")
  (:method ((ty t) response &key headers content)
    (write-response response 404 headers (or content "not found"))))

(defgeneric forbidden-response (ty response &key headers content)
  (:documentation "Constructs an HTTP 403 Forbidden response based on the specified content type.

- TY: A keyword indicating the desired response format. Supported values include
      :json, :html, and T (which defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.
- :HEADERS (optional): A plist of additional HTTP headers to include in the response.
- :CONTENT (optional): The body of the response. It will be serialized according to
                       the specified type. Defaults to \"Forbidden\".

Dispatches on the TY argument to format and serialize the response appropriately.
Custom methods should implement content handling for each supported type. The
default method (for TY = T) returns the given content (or a default message) with
a 403 status.")
  (:method ((ty t) response &key headers content)
    (write-response response 403 headers (or content "Forbidden"))))

(defgeneric unauthorized-response (ty response &key headers content)
  (:documentation "Constructs an HTTP 401 Unauthorized response based on the specified content type.

- TY: A keyword indicating the desired response format. Supported values include
      :json, :html, and T (which defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.
- :HEADERS (optional): A plist of additional HTTP headers to include in the response.
- :CONTENT (optional): The body of the response. It will be serialized according to
                       the specified type. Defaults to \"too many requests\".

Dispatches on the TY argument to format and serialize the response appropriately.
Custom methods should implement content handling for each supported type. The
default method (for TY = T) returns a 401 status with a default message of
\"unauthorized\".")
  (:method ((ty t) response &key headers content)
    (write-response response 401 headers "Unauthorized")))

(defgeneric bad-request-response (ty response &key headers content)
  (:documentation "Constructs an HTTP 400 Bad Request response based on the specified content type.

- TY: A keyword indicating the desired response format. Supported values include
      :json, :html, and T (which defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.
- :HEADERS (optional): A plist of additional HTTP headers to include in the response.
- :CONTENT (optional): The body of the response. It will be serialized according to
                       the specified type. Defaults to \"bad request\".

Dispatches on the TY argument to format and serialize the response appropriately.
Custom methods should implement content handling for each supported type. The
default method (for TY = T) returns a 400 status with a default message of
\"bad request\".")
  (:method ((ty t) response &key headers content)
    (write-response response 400 headers (or content "Bad request"))))

(defgeneric too-many-requests-response (ty response &key headers content)
  (:documentation "Constructs an HTTP 429 Too Many Requests response based on the specified content type.

- TY: A keyword indicating the desired response format. Supported values include
      :json, :html, and T (which defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.
- :HEADERS (optional): A plist of additional HTTP headers to include in the response.
- :CONTENT (optional): The body of the response. It will be serialized according to
                       the specified type. Defaults to \"too many requests\".

Dispatches on the TY argument to format and serialize the response appropriately.
The default method (for TY = T) returns a 429 status with the given content and
any provided headers.")
  (:method ((ty t) response &key headers content)
    (write-response response 429 headers (or content "too many requests"))))

(defgeneric redirect-see-other-response (ty response location &key headers content)
  (:documentation "Constructs an HTTP 303 See Other redirect response based on the specified content type.

- TY: A keyword indicating the desired response format. Supported values include
      :json, :html, and T (which defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.
- LOCATION: The URI to which the client is redirected, included in the Location header.
- :HEADERS (optional): A plist of additional HTTP headers to include in the response.
- :CONTENT (optional): The body of the response. It will be serialized according to
                       the specified type. Defaults to \"see other\".

Dispatches on the TY argument to format and serialize the response appropriately.
The default method (for TY = T) returns a 303 status with a 'see-other' message
and sets the Location header to the given URI.")
  (:method ((ty t) response location &key headers content)
    (write-response response
                    303
                    (append (list :location location) headers)
                    (or content "see other"))))

(defgeneric unprocessable-entity (ty response &key headers content)
  (:documentation "Constructs an HTTP 422 Unprocessable Entity response based on the specified content type.

- TY: A keyword indicating the desired response format. Supported values include
      :json, :html, and T (which defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.

Dispatches on the TY argument to format and serialize the response appropriately.
The default method (for TY = T) returns a 422 status with a default message of
\"unprocessable entity\".")
  (:method ((ty t) response &key headers content)
    (write-response response 422 headers (or content "unprocessable entity"))))

(defgeneric not-implemented (ty response &key headers content)
  (:documentation "Constructs an HTTP 501 Not Implemented response based on the specified content type.

- TY: A keyword indicating the desired response format. Supported values include
      :json, :html, and T (which defaults to HTML).
- RESPONSE: The response object used to build the final HTTP response.

Dispatches on the TY argument to format and serialize the response appropriately.
The default method (for TY = T) returns a 501 status with a default message of
\"not implemented\".")
  (:method ((ty t) response &key headers content)
    (write-response response 501 headers (or content "not implemented"))))
