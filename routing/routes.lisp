(in-package :io.github.cl-sdk.wst.routing)

(defstruct route
  "A Route object represents a single endpoint in the web server.

Fields:
- NAME: A symbol or string representing the unique name of the route.
- METHOD: The HTTP method expected (:GET, :POST, etc.).
- PATH: The URI path for the route, can include parameters (e.g., \"/users/:id\").
- MATCHER: Compiled matcher used internally to match request URIs.
- DATA: Holds information about parameterized routes (e.g., extracted params).
- CUSTOM: Additional data used by other components or middlewares.
- DISPATCHER: Function called with (request response) when this route matches."
  name
  method
  path
  matcher
  data
  custom
  dispatcher)

(defstruct matcher
  "Matcher is a cached object that holds information
to match a path efficiently.

Fields:
- METHOD: The HTTP method that the route expects (:GET, :POST, etc.).
- SEGMENTS-COUNT: Number of path segments, used for a quick mismatch check.
- SEGMENTS: List of each component of the path, split on `/`."
  method
  segments-count
  segments)

(defvar *not-fount-route*
  (make-route :name 'not-found
              :path nil
              :method nil
              :matcher nil
              :dispatcher #'not-found-response)
  "Global variable holding the default route to execute when no other route matches.
Defaults to using `wst.routing:not-found-response` as the dispatcher.")

(defvar *internal-error-route*
  (make-route :name 'internal-error
              :path nil
              :method nil
              :matcher nil
              :dispatcher #'internal-server-error-response)
  "Global variable holding the default route to execute when an internal server error occurs.
Defaults to using `wst.routing:internal-server-error-response` as the dispatcher.")

(defparameter *condition-handler* nil
  "Global variable holding a user-defined function that is called to handle conditions
before the default internal server error handler is invoked.")

(defparameter *any-route-handler* nil
  "Global variable holding a user-defined function that is called to handle any route
when no specific route matches.")

(defun condition-handler (fn)
  "Sets a user-defined function FN to handle conditions before invoking
the default internal server error handler."
  (setf *condition-handler* fn))

(defun %condition-stack-trace (err)
  (declare (ignorable err))
  (let* ((sb-debug-package (find-package :sb-debug))
         (print-backtrace (and sb-debug-package
                               (find-symbol "PRINT-BACKTRACE" sb-debug-package))))
    (when (and print-backtrace (fboundp print-backtrace))
      (ignore-errors
        (with-output-to-string (stream)
          ;; PRINT-BACKTRACE output varies by implementation; this targets
          ;; SBCL when available and returns NIL on unsupported Lisps.
          (let ((*debug-io* stream)
                (*error-output* stream)
                (*standard-output* stream)
                (*trace-output* stream))
            (funcall print-backtrace)))))))

(defun development-condition-handler (request response err)
  "Condition handler tuned for development/debugging.

Prints a detailed error message to `*error-output*` and returns it as the
500 response content."
  (let* ((stack-trace (%condition-stack-trace err))
         (message-template
           (concatenate 'string
                        "condition handled~%"
                        "=================~%"
                        "method: ~a~%"
                        "uri: ~a~%"
                        "type: ~a~%"
                        "message: ~a~%"
                        "~%"
                        "stack trace:~%"
                        "~a"))
         (message (format nil message-template
                          (request-method request)
                          (request-uri request)
                          (type-of err)
                          err
                          (if (and stack-trace (not (string= stack-trace "")))
                              stack-trace
                              "stack trace not available on this Lisp implementation"))))
    (format *error-output* "~&~a~%" message)
    (internal-server-error-response t response :content message)))

(defun any-route-handler (method fn)
  "Sets a user-defined function FN as the handler for all requests
matching METHOD when no specific route matches."
  (setf *any-route-handler*
        (make-route :name 'any-route
                    :method method
                    :path nil
                    :matcher nil
                    :dispatcher fn)))

(declaim (ftype (function (matcher symbol list integer) list)
                match))
(defun match (matcher method segments count)
  "Runs the MATCHER against a given HTTP METHOD and path SEGMENTS.

Parameters:
- MATCHER: The matcher object containing the expected method and path segments.
- METHOD: The HTTP method of the incoming request.
- SEGMENTS: A list of path segments from the incoming request URI.
- COUNT: Number of segments in the request URI.

Returns:
- (:params PARAMS) if the matcher succeeds, where PARAMS is an association list of path parameters.
- (:skip NIL) if the method or segment count does not match, or if any fixed segment mismatches."
  (if (or (not (= count (matcher-segments-count matcher)))
          (not (equal method (matcher-method matcher))))
      (list :skip nil)
      (list :params (loop :for x :in (matcher-segments matcher)
                          :for y :in segments
                          :if (str:starts-with? ":" x)
                            :collect (cons (str:substring 1 (length x) x) y)
                          :else :if (not (equal x y))
                                  :do (return-from match (list :skip nil))))))

(declaim (ftype (function (route symbol list integer) list)
                do-matcher))
(defun do-matcher (route method segments count)
  "Attempts to match a ROUTE against the given HTTP METHOD and path SEGMENTS.

Parameters:
- ROUTE: The route object to test.
- METHOD: The HTTP method of the incoming request.
- SEGMENTS: A list of path segments from the request URI.
- COUNT: Number of segments in the request URI.

Returns a cons cell (ROUTE . PARAMS) or NIL if the route does not match."
  (destructuring-bind (action params)
      (match (route-matcher route) method segments count)
    (when (equal action :params)
      (cons route params))))

(defun match-route (path method &optional (routes *routes*))
  "Finds a route from ROUTES that matches the given PATH and HTTP METHOD.

Parameters:
- PATH: The request URI as a string (e.g., \"/users/123\").
- METHOD: The HTTP method of the incoming request (:GET, :POST, etc.).
- ROUTES: Optional list of route objects to search; defaults to *ROUTES*.

Returns a cons cell (ROUTE . PARAMS) or NIL if no matching route is found."
  (let* ((segments
           (remove-if (lambda (p) (or (null p) (= 0 (length p))))
                      (cdr (str:split "/" path))))
         (count (length segments)))
    (loop :for route :in routes
          :do (alexandria:when-let ((match-data (do-matcher route method segments count)))
                (return match-data)))))

(declaim (ftype (function (string symbol) matcher)
                build-matcher))
(defun build-matcher (path method)
  "Constructs a matcher object for a given PATH and HTTP METHOD.

Parameters:
- PATH: The route URI as a string (e.g., \"/users/:id\").
- METHOD: The HTTP method the matcher should expect (:GET, :POST, etc.).

Returns a matcher object containing the method, segment count,
and path segments for efficient route matching."
  (let ((segments (remove-if (lambda (p) (or (null p) (= 0 (length p))))
                             (cdr (split "/" path)))))
    (make-matcher :method method
                  :segments-count (length segments)
                  :segments segments)))

(declaim (ftype (function (symbol string symbol function &optional list) t)
                add-route))
(defun add-route (name path method dispatcher &optional custom)
  "Adds a new route to the global *ROUTES* list.

Parameters:
- NAME: Symbol or string identifying the route.
- PATH: URI path for the route (can include parameters, e.g., \"/users/:id\").
- METHOD: HTTP method expected for this route (:GET, :POST, etc.).
- DISPATCHER: Function to handle requests matching this route.
- CUSTOM (optional): Additional data for use by other components or middlewares."
  (let ((route (make-route :name name
                           :path path
                           :method method
                           :matcher (build-matcher path method)
                           :dispatcher dispatcher
                           :custom custom)))
    (setf *routes* (append *routes* (list route)))
    t))

(defun parse-uri (uri)
  (let* ((hash-position (or (position #\# uri) (length uri)))
         (query-position (or (position #\? uri) hash-position)))
    (let ((path (str:substring 0 query-position uri))
          (query (str:substring (1+ query-position) hash-position uri))
          (hash (str:substring (1+ hash-position) (length uri) uri)))
      (values path query hash))))

(declaim (ftype (function (symbol) t)
                remove-route))
(defun remove-route (name)
  "Removes a route from the global *ROUTES* list by its NAME.

Parameters:
- NAME: Symbol or string identifying the route to remove."
  (setf *routes*
        (remove-if (lambda (route)
                     (equal name (route-name route)))
                   *routes*))
  t)

(declaim (ftype (function (symbol &optional list) (or route null))
                find-route-by-name))
(defun find-route-by-name (name &optional (routes *routes*))
  "Searches for a route in ROUTES by its NAME.

Parameters:
- NAME: Symbol identifying the route to find.
- ROUTES: Optional list of routes to search; defaults to *ROUTES*.

Returns a route object if found."
  (let ((sname (symbol-name name)))
    (find-if (lambda (route) (string-equal sname (symbol-name (route-name route)))) routes)))

(defun %dispatcher (route request response)
  "The dispatcher for any kind of dispatch.
 ROUTE-DATA is a pair of a route and the params and a request object."
  (labels ((default-internal-server-error-response (response)
             (setf (response-status response) 500
                   (response-headers response) (append (response-headers response)
                                                       (list :content-type "text/plain"))
                   (response-content response) "internal server error")
             response))
   (handler-case
       (let* ((fn (route-dispatcher (or route
                                       *not-fount-route*)))
              (rs (funcall fn request response)))
         rs)
     (t (err)
       (or (and *condition-handler* (funcall *condition-handler* request response err))
          (funcall #'default-internal-server-error-response response))))))

(defun dispatch-route (request)
  "Dispatches a route for the given REQUEST based on its PATH and METHOD.

Parameters:
- REQUEST: A request object containing URI, method, headers, and other data.

Behavior:
- Matches the request URI and method against registered routes.
- If a matching route is found, adds route and parameters to the request data
and calls the route's dispatcher.
- If no specific route matches but *ANY-ROUTE-HANDLER* is defined,
it is dispatched.
- If no route matches and no any-route handler exists, calls the default dispatcher
with NIL."
  (with-slots (method uri)
      request
    (let* ((response (make-response))
           (found (or (match-route uri method)
                     (and *any-route-handler*
                        (equal (request-method request) (route-method *any-route-handler*))
                        (cons *any-route-handler* nil)))))
      (if (not found)
          (%dispatcher nil request response)
          (destructuring-bind (route . params)
              found
            (progn
              (setf (request-data request)
                    (append (request-data request) (list :route route :params params)))
              (%dispatcher route request response)))))))

(defun dispatch-route-by-name (name request &optional old-params)
  "Dispatch a route by its PATH and METHOD. Pass REQUEST to it."
  (declare (ignorable old-params))
  (with-slots (method)
      request
    (let* ((response (make-response))
           (route (or (find-route-by-name name *routes*)
                     (and *any-route-handler*
                        (eql method (route-method *any-route-handler*))
                        *any-route-handler*)))
           (found (or (match-route (request-uri request) (request-method request)) (cons route nil))))
      (destructuring-bind (route . params)
          found
        (setf (request-data request) (append (request-data request) (list :params params)))
        (%dispatcher route request response)))))

(defun dispatch-route-by-route (route request)
  "Dispatch a route by its PATH and METHOD. Pass REQUEST to it."
  (let* ((response (make-response))
         (found (or (match-route (request-uri request) (request-method request) (list route))
                   (cons route nil))))
    (destructuring-bind (route . params)
        found
      (setf (request-data request) (append (request-data request) (list :params params)))
      (%dispatcher route request response))))

(defmacro route (name method path args &body body)
  "Macro to define a new route with a NAME, HTTP METHOD, and PATH.

Parameters:
- NAME: Symbol used as the function name for the route handler.
- METHOD: HTTP method this route responds to (:GET, :POST, etc.).
- PATH: The URI path for the route (can include parameters, e.g., \"/users/:id\").
- ARGS: Function arguments for the route handler (request response).
- BODY: Route handler.

Behavior:
- Removes any existing route with the same NAME.
- Defines a new function with NAME using ARGS and BODY.
- Registers the new route in *ROUTES* with the given PATH, METHOD,
and dispatcher pointing to the function."
  `(progn
     (remove-route ',name)
     (defun ,name ,args
       ,@body)
     (add-route ',name ,path ,method #',name)))

(defun route-uri-of (route args &key query)
  "Generates the URI for a given ROUTE, substituting ARGS
into any parameterized segments.

Parameters:
- ROUTE: The route object whose path will be used.
- ARGS: List of values to substitute into parameterized
segments (segments starting with ':').
- QUERY (optional): Query string to append to the URI
(e.g., \"page=2&sort=asc\").

Returns a string representing the full URI with parameters
and optional query string applied."
  (concatenate 'string "/"
               (str:join "/" (loop :for segment :in (matcher-segments (route-matcher route))
                                   :if (char-equal #\: (aref segment 0))
                                     :collect (format nil "~a" (pop args))
                                   :else
                                     :collect segment))
               (if query (concatenate 'string "?" query) "")))
