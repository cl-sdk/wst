# wst (web server tools)

![banner](./assets/banner.png)

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

# license

Unlicense.

See [license](https://github.com/cl-sdk/wst/blob/development/license).
