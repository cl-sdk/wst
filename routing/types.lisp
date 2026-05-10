(in-package :io.github.cl-sdk.wst.routing)

(defstruct request
  "Structure representing an HTTP request.

Slots:
  - URI: A string representing the requested URI.
  - QUERY: A string containing the query parameters.
  - HASH: A string representing the URI fragment (hash).
  - METHOD: A symbol representing the HTTP method (e.g., :GET, :POST). Defaults to :GET.
  - HEADERS: A hash-table of HTTP headers.
  - CONTENT-TYPE: A string or NIL indicating the content type of the request body.
  - CONTENT-LENGTH: An integer representing the length of the request content.
  - CONTENT: The raw content/body of the request.
  - DATA: A list holding parsed or additional data associated with the request."
  (uri "" :type string)
  (query "" :type string)
  (hash "" :type string)
  (method :GET :type symbol)
  (headers (hash-create nil) :type hash-table)
  (content-type nil :type (or string null))
  (content-length 0 :type integer)
  content
  (data nil :type list))

(defstruct response
  "Structure representing an HTTP response.

Slots:
  - STATUS: An integer representing the HTTP status code.
  - HEADERS: A list of HTTP headers as key-value pairs.
  - CONTENT: A string containing the response body.
  - DATA: A list for any additional data associated with the response."
  (status 0 :type integer)
  (headers nil :type list)
  (content "" :type string)
  (data nil :type list))

(defvar *routes* nil
  "Hash table storing all registered routes for request handling.")

(defmacro with-request-data (keys request &body body)
  "Bind values from KEYS extracted from the REQUEST's data into scope.

Arguments:
  - KEYS: A list of keys (symbols) to retrieve from the request data plist.
  - REQUEST: The request object containing the data plist.
  - BODY: Forms to execute with the keys bound to their corresponding values.

For each key in KEYS, the macro retrieves the value associated with that key
from the data plist of REQUEST and binds it to a variable of the same name,
making them available within BODY."
  (let ((ref (gensym "DATA")))
    `(let* ((,ref (request-data ,request))
            ,@(mapcar (lambda (item)
                        (list item
                              `(getf ,ref ,(intern (string item) :keyword))))
                      keys))
       ,@body)))

(defmacro with-response-data (keys response &body body)
  "Bind values from KEYS extracted from the RESPONSE's data into scope.

Arguments:
  - KEYS: A list of keys (symbols) to retrieve from the response data plist.
  - RESPONSE: The response object containing the data plist.
  - BODY: Forms to execute with the keys bound to their corresponding values.

For each key in KEYS, the macro retrieves the value associated with that key
from the data plist of RESPONSE and binds it to a variable of the same name,
making them available within BODY."
  (let ((ref (gensym "DATA")))
    `(let* ((,ref (response-data ,response))
            ,@(mapcar (lambda (item)
                        (list item
                              `(getf ,ref ,(intern (string item) :keyword))))
                      keys))
       ,@body)))

(defmacro with-request-params (keys params &body body)
  "Bind values from KEYS extracted from PARAMS into scope,
optionally applying transformation functions.

Arguments:
  - KEYS: A list where each element is either:
      - a symbol representing the parameter key to retrieve, or
      - a cons cell (KEY . FUNCTION) where FUNCTION is applied to the parameter value if present.
  - PARAMS: An association list or plist containing parameter key-value pairs, typically with string keys.
  - BODY: Forms to execute with the keys bound to their corresponding (and possibly transformed) values.

For each key in KEYS, the macro retrieves the parameter value from PARAMS by
case-insensitive string matching. If a transformation function is provided,
it is applied to the value before binding. The resulting bindings are available
within BODY."
  (let ((ref (gensym "PARAMS")))
    `(let* ((,ref ,params)
            ,@(mapcar (lambda (item)
                        (etypecase item
                          (symbol (list (intern (string item))
                                        `(alexandria:assoc-value ,ref
                                                                 ,(string-downcase (string item))
                                                                 :test
                                                                 #'string-equal)))
                          (cons (list (intern (string (car item)))
                                      `(let ((value (alexandria:assoc-value ,ref
                                                                            ,(string-downcase (string (car item)))
                                                                            :test
                                                                            #'string-equal)))
                                         (if value (funcall ,(cdr item) value) value))))))
                      keys))
       ,@body)))
