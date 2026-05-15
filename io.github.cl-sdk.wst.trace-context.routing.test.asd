(asdf:defsystem #:io.github.cl-sdk.wst.trace-context.routing.test
  :description "Tests for io.github.cl-sdk.wst.trace-context.routing."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.trace-context
               #:io.github.cl-sdk.wst.trace-context.routing
               #:io.github.cl-sdk.wst.routing)
  :pathname "t"
  :serial t
  :components ((:file "trace-context-routing-tests")))
