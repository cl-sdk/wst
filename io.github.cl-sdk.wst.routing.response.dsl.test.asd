(asdf:defsystem #:io.github.cl-sdk.wst.routing.response.dsl.test
  :description "Tests for io.github.cl-sdk.wst.routing.response.dsl."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.http
               #:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.response.dsl)
  :pathname "t"
  :serial t
  :components ((:file "response-dsl-tests")))
