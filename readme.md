# wst (web server tools)

Tools to help you build web applications in Common Lisp.

The project philosophy is explicitness: there is no hidden state.
There are no framework objects that secretly hold state for you.
Behavior is built from pipelines and composition, where middleware and handlers are combined in clear, predictable flows.

### available features

- Route registration and URI parameter handling
- Composable middleware and routing DSL (`group`, `wrap`, `resource`)
- HTTP request/response helpers
- Cookie parsing utilities
- Session and CSRF support
- Request content parsing middleware
- CORS middleware
- Static file routing
- Rate limiting with pluggable stores
- Circuit breaker core and routing middleware integration
- Accept-aware response selection helpers (`io.github.cl-sdk.wst.request-accept`)
- W3C Trace Context propagation (`traceparent`/`tracestate`) with routing adapter (`io.github.cl-sdk.wst.trace-context`)
- Feature flags following the [openfeature specification](https://openfeature.dev) and routing adapter (`io.github.cl-sdk.wst.feature-flag`)

Example route composition:

```lisp
(defparameter api
  `(:wrap
    :before (trace-context acquire-database-connection manage-session)
    :after (release-request-connection session->cookie write-cookies)
    :route (:group
             (:route :POST authenticate "/authenticate" authenticate-controller)
             (:resource "/api/v1"
               (:wrap
                :before (retrieve-authed-user)
                :route (:resource "/users"
                         (:route :GET api-get-users api-get-users-controller)
                           (:route :POST api-create-user api-create-user-controller)
                           (:route :GET api-retrieve-user-by-id "/:id" api-retrieve-user-by-id-controller))
                         (:resource "/groups"
                           (:route :GET api-list-groups api-list-groups-controller)
                           (:route :POST api-create-group api-create-group-controller)
                           (:route :GET api-retrieve-group-by-id "/:id" api-retrieve-group-by-id-controller)))))))
```

#### examples

You can find the examples at:

- [cl-sdk/wst-url-shortener-example](https://github.com/cl-sdk/wst-url-shortener-example)
- [cl-sdk/wst-bookmark-example](https://github.com/cl-sdk/wst-bookmark-example)

### `io.github.cl-sdk.wst.feature-flag` function examples

All examples below assume:

```lisp
(defpackage #:my-app
  (:use #:cl #:io.github.cl-sdk.wst.feature-flag))
(in-package #:my-app)
```

#### provider protocol

- `provider-name`
  ```lisp
  (provider-name (make-instance 'provider :name "my-provider"))
  ;; => "my-provider"
  ```
- `provider-metadata`
  ```lisp
  (provider-metadata (make-instance 'provider :name "my-provider"))
  ;; => (:name "my-provider")
  ```
- `initialize-provider`
  ```lisp
  (initialize-provider (make-instance 'provider :name "my-provider"))
  ;; => #<PROVIDER ...>
  ```
- `shutdown-provider`
  ```lisp
  (shutdown-provider (make-instance 'provider :name "my-provider"))
  ;; => #<PROVIDER ...>
  ```
- `resolve-boolean-details`
  ```lisp
  (resolve-boolean-details
   (make-instance 'provider :name "my-provider")
   "beta-flag" nil '(:user-id "u-1"))
  ;; => #S(EVALUATION-DETAILS ...)
  ```
- `resolve-string-details`
  ```lisp
  (resolve-string-details
   (make-instance 'provider :name "my-provider")
   "variant" "control" '(:user-id "u-1"))
  ;; => #S(EVALUATION-DETAILS ...)
  ```
- `resolve-number-details`
  ```lisp
  (resolve-number-details
   (make-instance 'provider :name "my-provider")
   "max-items" 10 '(:user-id "u-1"))
  ;; => #S(EVALUATION-DETAILS ...)
  ```
- `resolve-object-details`
  ```lisp
  (resolve-object-details
   (make-instance 'provider :name "my-provider")
   "config" '(:enabled nil) '(:user-id "u-1"))
  ;; => #S(EVALUATION-DETAILS ...)
  ```

#### client and API state

- `make-client`
  ```lisp
  (make-client :name "checkout" :domain "payments"
               :evaluation-context '(:app "checkout"))
  ;; => #S(FEATURE-FLAG-CLIENT ...)
  ```
- `create-client`
  ```lisp
  (create-client :name "checkout")
  ;; => #S(FEATURE-FLAG-CLIENT ...)
  ```
- `feature-flag-client-name`
  ```lisp
  (feature-flag-client-name (create-client :name "checkout"))
  ;; => "checkout"
  ```
- `feature-flag-client-domain`
  ```lisp
  (feature-flag-client-domain (create-client :domain "payments"))
  ;; => "payments"
  ```
- `feature-flag-client-evaluation-context`
  ```lisp
  (feature-flag-client-evaluation-context
   (create-client :evaluation-context '(:app "checkout")))
  ;; => (:app "checkout")
  ```
- `set-provider`
  ```lisp
  (set-provider (make-instance 'noop-provider :name "default"))
  ;; => #<NOOP-PROVIDER ...>
  ```
- `get-provider`
  ```lisp
  (get-provider)
  ;; => #<PROVIDER ...>
  ```
- `set-evaluation-context`
  ```lisp
  (set-evaluation-context '(:region "eu"))
  ;; => (:region "eu")
  ```
- `get-evaluation-context`
  ```lisp
  (get-evaluation-context)
  ;; => (:region "eu")
  ```
- `merge-evaluation-contexts`
  ```lisp
  (merge-evaluation-contexts '(:a 1 :shared :api)
                             '(:b 2 :shared :client)
                             '(:c 3 :shared :call))
  ;; => (:a 1 :b 2 :c 3 :shared :call)
  ```
- `reset-feature-flag`
  ```lisp
  (reset-feature-flag)
  ;; resets provider/domain/context global state
  ```

#### evaluation results and accessors

- `make-evaluation-details`
  ```lisp
  (make-evaluation-details :flag-key "beta-flag"
                           :value t
                           :reason *reason-static*
                           :variant "on")
  ;; => #S(EVALUATION-DETAILS ...)
  ```
- `evaluation-details-flag-key`
  ```lisp
  (evaluation-details-flag-key
   (make-evaluation-details :flag-key "beta-flag" :value t))
  ;; => "beta-flag"
  ```
- `evaluation-details-value`
  ```lisp
  (evaluation-details-value
   (make-evaluation-details :flag-key "beta-flag" :value t))
  ;; => T
  ```
- `evaluation-details-variant`
  ```lisp
  (evaluation-details-variant
   (make-evaluation-details :flag-key "beta-flag" :value t :variant "on"))
  ;; => "on"
  ```
- `evaluation-details-reason`
  ```lisp
  (evaluation-details-reason
   (make-evaluation-details :flag-key "beta-flag" :value t :reason *reason-static*))
  ;; => :STATIC
  ```
- `evaluation-details-error-code`
  ```lisp
  (evaluation-details-error-code
   (make-evaluation-details :flag-key "beta-flag" :value nil
                            :error-code *error-flag-not-found*))
  ;; => :FLAG-NOT-FOUND
  ```
- `evaluation-details-error-message`
  ```lisp
  (evaluation-details-error-message
   (make-evaluation-details :flag-key "beta-flag" :value nil
                            :error-message "missing"))
  ;; => "missing"
  ```
- `evaluation-details-metadata`
  ```lisp
  (evaluation-details-metadata
   (make-evaluation-details :flag-key "beta-flag" :value t
                            :metadata '(:source "cache")))
  ;; => (:source "cache")
  ```

#### typed evaluations

- `get-boolean-value`
  ```lisp
  (let ((client (create-client)))
    (get-boolean-value client "beta-flag" nil))
  ;; => T or NIL (provider-dependent)
  ```
- `get-string-value`
  ```lisp
  (let ((client (create-client)))
    (get-string-value client "variant" "control"))
  ;; => "control" or provider-returned string
  ```
- `get-number-value`
  ```lisp
  (let ((client (create-client)))
    (get-number-value client "max-items" 10))
  ;; => 10 or provider-returned number
  ```
- `get-object-value`
  ```lisp
  (let ((client (create-client)))
    (get-object-value client "config" '(:enabled nil)))
  ;; => (:enabled nil) or provider-returned object
  ```
- `get-boolean-details`
  ```lisp
  (let ((client (create-client)))
    (get-boolean-details client "beta-flag" nil))
  ;; => #S(EVALUATION-DETAILS ...)
  ```
- `get-string-details`
  ```lisp
  (let ((client (create-client)))
    (get-string-details client "variant" "control"))
  ;; => #S(EVALUATION-DETAILS ...)
  ```
- `get-number-details`
  ```lisp
  (let ((client (create-client)))
    (get-number-details client "max-items" 10))
  ;; => #S(EVALUATION-DETAILS ...)
  ```
- `get-object-details`
  ```lisp
  (let ((client (create-client)))
    (get-object-details client "config" '(:enabled nil)))
  ;; => #S(EVALUATION-DETAILS ...)
  ```

### `io.github.cl-sdk.wst.feature-flag.routing` function examples

All examples below assume:

```lisp
(defpackage #:my-app.routing
  (:use #:cl
        #:io.github.cl-sdk.wst.feature-flag
        #:io.github.cl-sdk.wst.feature-flag.routing))
(in-package #:my-app.routing)
```

- `wrap-feature-flag-context`
  ```lisp
  (defparameter *middleware*
    (wrap-feature-flag-context
     :context-fn (lambda (request)
                   (let ((tenant (or (getf (io.github.cl-sdk.wst.routing:request-data request) :tenant)
                                     "acme")))
                     (list :tenant tenant :plan "pro")))))
  ```
- `feature-flag-context-of`
  ```lisp
  (feature-flag-context-of request)
  ;; => (:tenant "acme" :plan "pro")
  ```
- `client-for-request`
  ```lisp
  (let ((client (client-for-request request
                                    :domain "payments"
                                    :evaluation-context '(:app "checkout"))))
    (get-boolean-value client "new-checkout" nil))
  ```

# license

Unlicense.

See [license](https://github.com/cl-sdk/wst/blob/development/license).
