(asdf:defsystem #:io.github.cl-sdk.wst.request-content.routing.test
  :description "Tests for io.github.cl-sdk.wst.request-content.routing."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.request-content
               #:io.github.cl-sdk.wst.request-content.routing
               #:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.dsl)
  :pathname "t"
  :serial t
  :components ((:file "request-content-routing-tests")))
