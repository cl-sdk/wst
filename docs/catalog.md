# wst – System Catalog

> Auto-generated inventory of every ASDF system in this repository.
> See [package-map.md](package-map.md) for the old→new ownership and dependency flow.

---

## 1. System Inventory

| System | Source directory | `.asd` file |
|---|---|---|
| `wst.routing` | `routing/` | `wst.routing.asd` |
| `wst.routing.dsl` | `routing/` | `wst.routing.dsl.asd` |
| `wst.routing.response.dsl` | `routing/` | `wst.routing.response.dsl.asd` |
| `wst.routing.static` | `static/` | `wst.routing.static.asd` |
| `wst.routing.woo` | `adapters/` (was `web-server/`) | `wst.routing.woo.asd` |
| `wst.http` | `http/` | `wst.http.asd` |
| `wst.cookies` | `cookies/` | `wst.cookies.asd` |
| `wst.session` | `session/` | `wst.session.asd` |
| `wst.session.csrf` | `session/` (was `web-server/`) | `wst.session.csrf.asd` |
| `wst.routing.test` | `t/` | `wst.routing.test.asd` |

---

## 2. Feature Catalog

### 2.1 Routing core – `wst.routing`

**Purpose**: Register, match, and dispatch HTTP routes.

**Key exports**

| Symbol | Kind | Description |
|---|---|---|
| `request` | struct | HTTP request (URI, query, hash, method, headers, content-type, content-length, content, data) |
| `response` | struct | HTTP response (status, headers, content, data) |
| `make-request` / `make-response` | constructors | Create fresh request / response objects |
| `request-uri`, `request-query`, `request-method`, `request-headers`, `request-content`, `request-content-type`, `request-content-length`, `request-data` | accessors | Read slots of `request` |
| `response-status`, `response-headers`, `response-content`, `response-data` | accessors | Read slots of `response` |
| `add-route` | function | Register a named route (name, path, method, dispatcher, optional custom) |
| `remove-route` | function | Unregister a route by name |
| `find-route-by-name` | function | Look up a route object by name |
| `dispatch-route` | function | Match request URI/method and call the dispatcher |
| `dispatch-route-by-name` | function | Dispatch a route looked up by name |
| `dispatch-route-by-route` | function | Dispatch a specific route object |
| `route-uri-of` | function | Build a URI string from a route and argument list |
| `route-path` | accessor | The raw path string of a route |
| `parse-uri` | function | Split a URI string into (path, query, hash) values |
| `condition-handler` | function | Set a global error-handling callback |
| `any-route-handler` | function | Set a catch-all handler for a given HTTP method |
| `with-request-data` | macro | Destructure keys from `request-data` |
| `with-response-data` | macro | Destructure keys from `response-data` |
| `with-request-params` | macro | Destructure URI parameters (with optional transforms) |
| `write-response` | function | Write status/headers/content onto a `response` object |
| `ok-response` | generic | Build a 200 OK response |
| `created-response` | generic | Build a 201 Created response |
| `not-found-response` | generic | Build a 404 Not Found response |
| `bad-request-response` | generic | Build a 400 Bad Request response |
| `unauthorized-response` | generic | Build a 401 Unauthorized response |
| `forbidden-response` | generic | Build a 403 Forbidden response |
| `redirect-see-other-response` | generic | Build a 303 See Other response |
| `unprocessable-entity` | generic | Build a 422 Unprocessable Entity response |
| `internal-server-error-response` | generic | Build a 500 Internal Server Error response |
| `not-implemented` | generic | Build a 501 Not Implemented response |
| `route` | macro | Define a route + handler function in one form |

**External dependencies**: `alexandria`, `str`, `cl-hash-util`, `flexi-streams`, `serapeum`, `com.inuoe.jzon`

**Test coverage**: ✅ (via `wst.routing.test`, `wst.routing.suite`)

---

### 2.2 Routing DSL – `wst.routing.dsl`

**Purpose**: Compose routes with middleware using a declarative tree syntax.

**Key exports**

| Symbol | Kind | Description |
|---|---|---|
| `build-webserver` | function | Compile an API tree into registered routes |
| `route` | macro form | `(route METHOD NAME URI-OR-HANDLER HANDLER [:custom …])` |
| `group` | macro form | Combine several route forms without affecting paths |
| `resource` | macro form | Scope nested routes under a common URI prefix |
| `wrap` | macro form | Add `:before` / `:after` middleware to a route or group |
| `any-route` | macro form | Register a catch-all handler for a method |

**Middleware contract**: before-handlers must return `(cons :continue response)` or `(cons :halt response)`.

**External dependencies**: `alexandria`, `str`, `cl-hash-util`, `wst.routing`

**Test coverage**: ✅ (tested inside `wst.routing.suite`)

---

### 2.3 Response DSL – `wst.routing.response.dsl`

**Purpose**: Fluent helpers for setting response content and headers.

**Key exports**

| Symbol | Kind | Description |
|---|---|---|
| `status` | function | Set `response-status` |
| `headers` | function | Merge key-value pairs into `response-headers` |
| `text` | function | Set content + `Content-Type: text/plain` |
| `html` | generic | Set content + `Content-Type: text/html` (dispatches on implementation) |
| `json` | generic | Set content + `Content-Type: application/json` (dispatches on implementation) |

**External dependencies**: `wst.routing`

**Test coverage**: ✅ (`wst.routing.response.dsl.suite`)

---

### 2.4 HTTP status constants – `wst.http`

**Purpose**: Machine-readable HTTP status codes and descriptions.

**Pattern**: For every status code N, two symbols are exported:
- `+http-status-N+` – integer constant (e.g. `200`)
- `http-status-N` – list `(code "Reason" "Description" "MDN-URL")`

**Codes covered**: 100–103, 200–206, 300–308, 400–431, 451, 500–511.

**Generation**: `generate-http-status.lisp` reads `data/http_statuses.csv` and writes `http/package.lisp`.

**External dependencies**: none

**Test coverage**: ❌ (no dedicated test suite; used by `wst.routing.response.dsl.suite` via `+http-status-200+`)

---

### 2.5 Woo adapter – `wst.routing.woo`

**Purpose**: Bridge between the [Woo](https://github.com/fukamachi/woo) HTTP server environment and the `wst.routing` request/response model.

**Key exports**

| Symbol | Kind | Description |
|---|---|---|
| `request-from-woo-env` | function | Translate a Woo `env` plist into a `wst.routing:request` |
| `response-to-woo-response` | function | Translate a `wst.routing:response` into `(status headers body)` |

**External dependencies**: `wst.routing` *(was missing from `.asd`; fixed in Phase 2)*

**Source file**: `adapters/woo.lisp` *(moved from `web-server/` in Phase 3)*

**Test coverage**: ❌ (no dedicated suite)

---

### 2.6 Session contracts – `wst.session`

**Purpose**: Generic interface for session backends.

**Key exports**

| Symbol | Kind | Description |
|---|---|---|
| `create-session` | generic | Create a new session |
| `recover-session` | generic | Load a session by id |
| `update-session` | generic | Persist session changes |
| `session-exists-p` | generic | Check whether a session is still active |
| `renew-session` | generic | Extend session expiry |
| `terminate-session` | generic | Destroy a session |

**External dependencies**: none

**Test coverage**: ❌

---

### 2.7 Cookie-backed session helpers – `wst.cookies`

**Purpose**: Generic interface for a cookie / session-store backend.

> **Note**: Despite the package name `wst.cookies`, the exported API is entirely session-lifecycle-oriented. The cookies package in `wst.routing` core handles cookie *parsing*; `wst.cookies` handles session *persistence*. See §4 for the proposed rename.

**Key exports**

| Symbol | Kind | Description |
|---|---|---|
| `initialize-session` | generic | Start a new session |
| `recover-session` | generic | Restore an existing session |
| `update-session` | generic | Persist session state |
| `terminate-session` | generic | End a session |

**External dependencies**: none

**Test coverage**: ❌

---

### 2.8 CSRF tokens – `wst.session.csrf`

**Purpose**: Generic interface for managing per-session CSRF tokens.

**Key exports**

| Symbol | Kind | Description |
|---|---|---|
| `session-csrf-token` | generic | Get the current CSRF token |
| `add-session-csrf-token` | generic | Issue a new token |
| `remove-session-csrf-token` | generic | Invalidate a token |
| `verify-session-csrf-token` | generic | Check a submitted token |

**External dependencies**: none *(implementation delegates to the consumer)*

**Source file**: `session/csrf.lisp` *(moved from `web-server/` in Phase 3)*

**Test coverage**: ❌

---

### 2.9 Static file serving – `wst.routing.static`

**Purpose**: Macro helper to register GET routes that serve files from a configurable directory.

**Key exports**

| Symbol | Kind | Description |
|---|---|---|
| `change-static-path` | function | Set the base directory for static files |
| `route-static` | macro | Define a named GET route serving a file with a given MIME type |

**External dependencies**: `wst.routing`

**Test coverage**: ❌

---

### 2.10 Tests – `wst.routing.test`

**Purpose**: FiveAM test suite for `wst.routing`, `wst.routing.dsl`, `wst.routing.woo`, and `wst.routing.response.dsl`.

**Test suites**

| Suite | Coverage |
|---|---|
| `wst.routing.suite` | Route add/remove, dispatch, params, cookies, condition handler, any-route, URI parse |
| `wst.routing.response.dsl.suite` | headers, status, text, html, json |

**External dependencies**: `alexandria`, `str`, `serapeum`, `cl-hash-util`, `fiveam`, `wst.http`, `wst.routing`, `wst.routing.dsl`, `wst.routing.woo`, `wst.routing.response.dsl`

**Run tests**: `make tests`

---

## 3. Problem Areas & Ambiguities

| # | Problem | Affected systems | Severity |
|---|---|---|---|
| P1 | `wst.routing.woo` declares no `:depends-on` yet calls `wst.routing:parse-uri`, `wst.routing:make-request` | `wst.routing.woo` | **High** – loading order not guaranteed |
| P2 | `wst.session.csrf` source lives in `web-server/` directory | `wst.session.csrf` | **Medium** – directory name contradicts system name |
| P3 | `wst.routing.woo` source lives in `web-server/` directory | `wst.routing.woo` | **Medium** – no `adapters/` concept exists |
| P4 | `wst.routing.response.dsl` uses `:module` form; all others use `:pathname` | `wst.routing.response.dsl` | **Low** – inconsistent style |
| P5 | `wst.http` and `wst.routing.response.dsl` have no `:license` or `:version` | both | **Low** – incomplete metadata |
| P6 | `wst.cookies` exports session lifecycle API under a "cookies" name | `wst.cookies` | **Low** – naming confusion vs cookie parsing in `wst.routing` |
| P7 | `t/package.lisp` mixes two test packages (`wst.routing.test` and `wst.routing.response.dsl.test`) in a single file | `wst.routing.test` | **Low** – harder to navigate |
