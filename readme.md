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

Example route composition:

```lisp
(defparameter api
  `(:wrap
    :before (list acquire-request-connection manage-session)
    :after (list http-response-cookies->set-cookie release-request-connection)
    :route (:resource "/api/v1"
                      (:route :POST api-sign-up "/sign-up" api-sign-up-controller)
                      (:route :POST api-log-in "/log-in" api-log-in-controller))))
```

### Accept-driven responses

Use route `:custom` metadata to declare response media types and use
`io.github.cl-sdk.wst.request-accept:respond` in handlers:

```lisp
(:route :get users "/users"
        (lambda (request response)
          (io.github.cl-sdk.wst.request-accept:respond
           users
           request
           response))
        :custom (list :response-accepts '(:|application/json| :|text/html|)))
```

Override `respond-with` methods per media type to render content as needed.

#### examples

You can find the examples at:

- [cl-sdk/wst-url-shortener-example](https://github.com/cl-sdk/wst-url-shortener-example)
- [cl-sdk/wst-bookmark-example](https://github.com/cl-sdk/wst-bookmark-example)

# license

Unlicense.

See [license](https://github.com/cl-sdk/wst/blob/development/license).
