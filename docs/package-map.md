# wst – Package Map

> Canonical reference showing old→new ownership and the dependency graph after the cleanup.

---

## Directory / System / Package alignment

| ASDF system | Lisp package | Directory (old → new) | Source file(s) |
|---|---|---|---|
| `wst.routing` | `wst.routing` | `routing/` (unchanged) | `package.lisp`, `types.lisp`, `responses.lisp`, `routes.lisp` |
| `wst.routing.dsl` | `wst.routing.dsl` | `routing/` (unchanged) | `dsl.lisp` |
| `wst.routing.response.dsl` | `wst.routing.response.dsl` | `routing/` (unchanged) | `response-dsl.lisp` |
| `wst.routing.static` | `wst.routing.static` | `static/` (unchanged) | `package.lisp` |
| `wst.routing.woo` | `wst.routing.woo` | `web-server/` → `adapters/` | `woo.lisp` |
| `wst.http` | `wst.http` | `http/` (unchanged) | `package.lisp` |
| `wst.cookies` | `wst.cookies` | `cookies/` (unchanged) | `package.lisp` |
| `wst.session` | `wst.session` | `session/` (unchanged) | `package.lisp` |
| `wst.session.csrf` | `wst.session.csrf` | `web-server/` → `session/` | `session-csrf.lisp` → `csrf.lisp` |
| `wst.routing.test` | `wst.routing.test` | `t/` (unchanged) | `package.lisp` |

---

## Dependency graph

```
wst.http
   (no dependencies)

wst.cookies
   (no dependencies)

wst.session
   (no dependencies)

wst.session.csrf
   (no dependencies — pure generic interface; implementations depend on it)

wst.routing
   ├── alexandria
   ├── str
   ├── cl-hash-util
   ├── flexi-streams
   ├── serapeum
   └── com.inuoe.jzon

wst.routing.dsl
   ├── wst.routing
   ├── alexandria
   ├── str
   └── cl-hash-util

wst.routing.response.dsl
   └── wst.routing

wst.routing.static
   └── wst.routing

wst.routing.woo
   └── wst.routing          ← dependency was MISSING before Phase 2 fix

wst.routing.test
   ├── wst.http
   ├── wst.routing
   ├── wst.routing.dsl
   ├── wst.routing.woo
   ├── wst.routing.response.dsl
   ├── alexandria
   ├── str
   ├── serapeum
   ├── cl-hash-util
   └── fiveam
```

---

## Domain grouping

```
adapters/
  woo.lisp              ← wst.routing.woo  (Woo HTTP server bridge)

cookies/
  package.lisp          ← wst.cookies      (cookie-backed session store interface)

http/
  package.lisp          ← wst.http         (HTTP status constants 100–511)

routing/
  package.lisp  \
  types.lisp     |      ← wst.routing      (core route registration & dispatch)
  responses.lisp |
  routes.lisp   /
  dsl.lisp              ← wst.routing.dsl  (group/resource/wrap DSL)
  response-dsl.lisp     ← wst.routing.response.dsl  (response fluent helpers)

session/
  package.lisp          ← wst.session      (session backend interface)
  csrf.lisp             ← wst.session.csrf (CSRF token interface)

static/
  package.lisp          ← wst.routing.static  (static file route helper)

t/
  package.lisp          ← wst.routing.test (FiveAM test suites)
```

---

## Changelog summary

| Phase | Change | Breakage |
|---|---|---|
| 1 | Created `docs/catalog.md` (this catalog) | None |
| 2 | Added `:description` to all 10 `.asd` files | None |
| 2 | Added `:license "Unlicense"` and `:version "0.0.1"` to `wst.http` and `wst.routing.response.dsl` | None |
| 2 | Added `:depends-on (#:wst.routing)` to `wst.routing.woo.asd` | None (fixes load order) |
| 2 | Replaced `:module` form with `:pathname` in `wst.routing.response.dsl.asd` | None |
| 3 | Moved `web-server/woo.lisp` → `adapters/woo.lisp`; updated `wst.routing.woo.asd` | None (ASDF controls lookup) |
| 3 | Moved `web-server/session-csrf.lisp` → `session/csrf.lisp`; updated `wst.session.csrf.asd` | None (ASDF controls lookup) |
| 4 | Removed now-empty `web-server/` directory | None |
