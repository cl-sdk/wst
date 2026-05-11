(asdf:defsystem #:io.github.cl-sdk.wst.routing.dsl.test
  :description "Tests for io.github.cl-sdk.wst.routing.dsl."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.circuit-breaker.routing
               #:io.github.cl-sdk.wst.rate-limit
               #:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.dsl)
  :pathname "t"
  :serial t
  :components ((:file "routing-dsl-tests")))
