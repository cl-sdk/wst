(asdf:defsystem #:io.github.cl-sdk.wst.test
  :description "Umbrella FiveAM test system for all io.github.cl-sdk.wst test subsystems."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.routing.test
               #:io.github.cl-sdk.wst.routing.dsl.test
               #:io.github.cl-sdk.wst.circuit-breaker.test
               #:io.github.cl-sdk.wst.rate-limit.test
               #:io.github.cl-sdk.wst.cors.test
               #:io.github.cl-sdk.wst.routing.woo.test
               #:io.github.cl-sdk.wst.routing.response.dsl.test
               #:io.github.cl-sdk.wst.request-content.test
               #:io.github.cl-sdk.wst.request-content.routing.test
               #:io.github.cl-sdk.wst.request-accept.test
               #:io.github.cl-sdk.wst.trace-context.test
               #:io.github.cl-sdk.wst.trace-context.routing.test
               #:io.github.cl-sdk.wst.feature-flag.test
               #:io.github.cl-sdk.wst.feature-flag.routing.test
               #:io.github.cl-sdk.wst.session.sqlite.test
               #:io.github.cl-sdk.wst.rate-limit.redis-store.test))
