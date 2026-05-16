(asdf:defsystem #:io.github.cl-sdk.wst.openfeature.routing.test
  :description "Tests for io.github.cl-sdk.wst.openfeature.routing."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.openfeature
               #:io.github.cl-sdk.wst.openfeature.routing
               #:io.github.cl-sdk.wst.routing)
  :pathname "t"
  :serial t
  :components ((:file "openfeature-routing-tests")))
