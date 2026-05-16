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
- Feature flags with core API/client/provider model and routing adapter (`io.github.cl-sdk.wst.feature-flag`)

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

### feature-flag architecture and scope

The feature-flag implementation is split into:

- Core (`io.github.cl-sdk.wst.feature-flag`): API state, providers, clients, evaluation context merge, typed evaluations, and evaluation details.
- Routing adapter (`io.github.cl-sdk.wst.feature-flag.routing`): request-scoped evaluation context injection and request-aware client composition.

Current MVP scope:

- API-level provider and per-domain provider registration.
- Client creation with optional domain and client evaluation context.
- Evaluation context merge precedence: API context -> client context -> invocation context.
- Typed evaluations for boolean, string, number, and object.
- Evaluation details including reason and error metadata.
- Default/no-op provider fallback behavior.

### provider lifecycle and contract

Provider authors implement one or more typed resolver generics:

- `resolve-boolean-details`
- `resolve-string-details`
- `resolve-number-details`
- `resolve-object-details`

Optional lifecycle generics:

- `initialize-provider`
- `shutdown-provider`

A provider returns `evaluation-details` objects with at least a `value`, and optionally `variant`, `reason`, `error-code`, `error-message`, and `metadata`.

### routing usage

Use `wrap-feature-flag-context` as a `:before` middleware to inject request-scoped evaluation context into `request-data`.

Use `client-for-request` to build a client whose context includes request-scoped values plus any explicit client/request invocation context.

### constraints

- Core package has zero routing dependency.
- Routing concerns are isolated in `io.github.cl-sdk.wst.feature-flag.routing`.
- Context handling is explicit (plists only), with deterministic precedence.

### phase 2+ roadmap

- Hooks pipeline (`before` / `after` / `error` / `finally`) with immutable context transition rules.
- Provider status/events and richer lifecycle management.
- Extended propagation patterns (transaction/request integrations).
- Optional advanced additions like tracking and structured diagnostics.

# license

Unlicense.

See [license](https://github.com/cl-sdk/wst/blob/development/license).
