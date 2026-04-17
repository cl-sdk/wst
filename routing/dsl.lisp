(defpackage #:wst.routing.dsl
  (:use #:cl)
  (:documentation "Routing DSL for building webserver endpoints.

Available constructs:

  • ROUTE
    Define a single route.

    Syntax:
      (route REQUEST-METHOD ROUTE-NAME ROUTE-URI-OR-HANDLER &rest HANDLER)

    - REQUEST-METHOD        – HTTP method keyword (e.g., :get, :post).
    - ROUTE-NAME            – Symbol identifying the route.
    - ROUTE-URI-OR-HANDLER  – Either:
                                • A string (the route URI), with the first
                                  element of HANDLER taken as the responder.
                                • A function to be used directly as the responder.
    - HANDLER               – One or more handler functions, may include :custom
                              metadata following the responder.

    Behavior:
      Registers a named route with the given METHOD and URI, composed of
      optional pre-handlers, the main responder, and post-handlers.


  • GROUP
    Group multiple routes into a single unit.

    Syntax:
      (group &rest ROUTES)

    - ROUTES – A sequence of route forms (e.g., route, resource, wrap, any-route).

    Behavior:
      Groups routes together without altering paths or handlers.
      Useful for structuring related routes.


  • RESOURCE
    Define a resource scope with a common URI prefix and nested routes.

    Syntax:
      (resource ROUTE-URI &rest ROUTES)

    - ROUTE-URI – A string appended to the current URI segments.
    - ROUTES    – Nested route definitions inside this resource.

    Behavior:
      Temporarily extends the URI stack with ROUTE-URI, defines the nested ROUTES,
      then restores the previous URI stack. Enables hierarchical, REST-like route
      definitions.


  • WRAP
    Wrap a route or group of routes with before/after middleware.

    Syntax:
      (wrap :before BEFORE-FNS :after AFTER-FNS :route ROUTE)

    - :before – A list of functions to run before the ROUTE.
    - :after  – A list of functions to run after the ROUTE.
    - :route  – A route definition or a group of routes.

    Behavior:
      Temporarily augments the handler stack with BEFORE-FNS and AFTER-FNS when
      building ROUTE, then restores the stack.

    Example:
      (wrap :before (list fn1 fn2)
            :after  (list fn4 fn3)
            :route  route)

    Execution order:
      fn1 → fn2 → route → fn4 → fn3.


  • ANY-ROUTE
    Define a fallback handler executed when no other route matches.

    Syntax:
      (any-route REQUEST-METHOD HANDLER)

    - REQUEST-METHOD – HTTP method keyword (e.g., :get, :post).
    - HANDLER        – A function accepting REQUEST and RESPONSE.

    Behavior:
      Registers a route that always matches for the given METHOD if no specific
      route handled the request earlier. Useful for default handlers or catch-all
      endpoints.")
  (:import-from #:cl-hash-util
                #:hash
                #:with-keys)
  (:import-from #:wst.routing
                #:any-route-handler)
  (:import-from #:alexandria
                #:ensure-list)
  (:import-from #:wst.routing
                #:add-route)
  (:import-from #:wst.routing
                #:remove-route)
  (:import-from #:str
                #:join)
  (:export
   #:build-webserver
   #:wrap
   #:any-route
   #:route
   #:group
   #:resource))

(in-package #:wst.routing.dsl)

(defun %handler-executor (handler before-actions after-actions)
  (lambda (request response)
    (let ((halted nil))
      (dolist (fn before-actions)
        (destructuring-bind (control . response)
            (funcall fn request response)
          (cond
            ((eq control :halt) (progn
                                  (setf halted t)
                                  (return)))
            ((eq control :continue) t)
            (t (progn (print control) (error "middleware must return a pair of (:halt | :continue, response)"))))))
      (unless halted
        (funcall handler request response))
      (dolist (fn after-actions)
        (funcall fn request response))
      response)))

(defun %any-route (api stack)
  "Construct a route handler from an API definition and a STACK.

Arguments:
  API   – A list where the first element is the HTTP method keyword
          (e.g., :get, :post), followed by route-specific handler functions.
  STACK – A structure containing:
            \"fns-pre-handler\"  – Functions to run before the main handlers.
            \"uri-segments\"     – URI segments from nested routes (unused here).
            \"fns-post-handler\" – Functions to run after the main handlers.

  Composes before, API-provided, and after handlers into a sequence of actions.
  Defines a route handler "
  (with-keys ((pre-handler "fns-pre-handler") (paths "uri-segments") (post-handler "fns-post-handler"))
    stack
    (destructuring-bind (method &rest rest)
        api
      (any-route-handler
       method
       (%handler-executor (car rest)
                          (reduce #'append pre-handler)
                          (reduce #'append post-handler))))))

(defun %create-route (api stack)
  "Create and register a route from an API definition and a STACK.

Arguments:
  API   – A list in the form:
            (METHOD ROUTE-NAME PATH &rest REST)
          where:
            • METHOD      – HTTP method keyword (e.g., :get, :post).
            • ROUTE-NAME  – Symbolic name identifying the route.
            • PATH        – Either a string (route path) or a handler function.
                             - If PATH is a string, the first element of REST is
                               the handler.
                             - If PATH is not a string, it is taken as the handler.
            • REST        – Remaining arguments, may include :custom metadata.
  STACK – A structure containing:
            \"fns-pre-handler\"  – Functions to run before the main handler(s).
            \"uri-segments\"     – Path segments accumulated from resources.
            \"fns-post-handler\" – Functions to run after the main handler(s).

Behavior:
  - Builds the full route path by concatenating accumulated URI segments with
    PATH if it is a string, otherwise leaves it empty.
  - Selects the main handler according to PATH and REST.
  - Combines pre-handlers, the main handler, and post-handlers into a single
    sequence of ACTIONS.
  - Removes any existing route with the same ROUTE-NAME, then registers a new
    route with METHOD, ROUTE-NAME, and the computed path.
  - The registered route executes ACTIONS in order with REQUEST and RESPONSE,
    returning the final RESPONSE.
  - Any trailing :CUSTOM keyword and its value(s) are stored as route metadata."
  (with-keys ((pre-handler "fns-pre-handler") (paths "uri-segments") (post-handler "fns-post-handler"))
    stack
    (destructuring-bind (method route-name path &rest rest)
        api
      (let* ((onstack (join "" paths))
             (route-path (if (stringp path) path ""))
             (the-path (concatenate 'string onstack route-path))
             (handler (if (not (stringp path)) path (car rest))))
        (remove-route route-name)
        (add-route route-name the-path method
                   (%handler-executor handler
                                      (reduce #'append pre-handler)
                                      (reduce #'append post-handler))
                   (cdr (member :custom rest)))))))

(defun %wrap-routes (api stack)
  "Wrap a route or group of routes with before/after middleware functions.

Arguments:
  API   – A plist that may contain the following keys:
            • :before – A list of functions to run before the route.
            • :route  – The actual route definition or routing group.
            • :after  – A list of functions to run after the route.
  STACK – A structure containing:
            \"fns-pre-handler\"  – Accumulated pre-handler functions.
            \"fns-post-handler\" – Accumulated post-handler functions.

Behavior:
  - Temporarily extends the current STACK by appending the :before functions
    to the pre-handler list and the :after functions to the post-handler list.
  - Delegates to `%build-webserver` with the provided :route definition.
  - Restores the pre-handler and post-handler lists to their previous state
    after processing the route or routing group.

Example:
  (wrap :before (list fn1 fn2)
        :after  (list fn4 fn3)
        :route  route)

Execution order in this example:
  fn1 → fn2 → route → fn3 → fn4"
  (with-keys ((pre-handler "fns-pre-handler") (post-handler "fns-post-handler"))
      stack
    (progn
      (setf pre-handler (append pre-handler (list (ensure-list (getf api :before nil))))
            post-handler (append post-handler (list (ensure-list (getf api :after nil)))))
      (%build-webserver (getf api :route) stack)
      (setf pre-handler (butlast pre-handler)
            post-handler (butlast post-handler)))))

(defun %build-webserver (api stack)
  "Build the webserver routing tree from an API definition and a STACK.

Arguments:
  API   – A list beginning with a keyword indicating the type of routing
          construct, followed by its arguments:
            • (wrap ...)     – Wrap a route or group with before/after handlers.
            • (any-route ...) – Define a generic route from method and handlers.
            • (route ...)     – Define a named route with path and handler.
            • (group ...)     – Group multiple API definitions together.
            • (resource PATH ...) – Define a resource, extending the current
              URI segments with PATH, and containing nested routes or groups.
  STACK – A structure carrying accumulated context for building routes, such as
          pre-handlers, post-handlers, and URI segments.

Behavior:
  - Dispatches to the appropriate builder function depending on the first
    element of the API definition:
      • wrap      → `%wrap-routes`
      • any-route → `%any-route`
      • route     → `%create-route`
      • group     → Recursively builds each child API definition.
      • resource  → Temporarily extends the \"uri-segments\" in STACK with
                    the given PATH, builds the nested definitions, and then
                    restores the previous segments.
  - Constructs the complete routing tree by combining these definitions."
  (let ((item (car api))
        (routes (cdr api)))
    (case item
      (wrap (%wrap-routes routes stack))
      (any-route (%any-route routes stack))
      (route (%create-route routes stack))
      (group (map nil (lambda (api) (%build-webserver api stack)) routes))
      (resource (destructuring-bind (path &rest rest)
                    (cdr api)
                  (setf (gethash "uri-segments" stack)
                        (append (gethash "uri-segments" stack) (list path)))
                  (map nil (lambda (item) (%build-webserver item stack)) rest)
                  (setf (gethash "uri-segments" stack)
                        (butlast (gethash "uri-segments" stack))))))))

(defun build-webserver (api)
  "Build web server from a API definition."
  (%build-webserver api (hash ("uri-segments"  nil) ("fns-pre-handler"  nil) ("fns-post-handler"  nil))))
