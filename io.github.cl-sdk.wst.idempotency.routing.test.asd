(asdf:defsystem #:io.github.cl-sdk.wst.idempotency.routing.test
  :description "Tests for io.github.cl-sdk.wst.idempotency.routing."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.routing.dsl
               #:io.github.cl-sdk.wst.idempotency.routing)
  :pathname "t"
  :serial t
  :components ((:file "idempotency-routing-tests")))
