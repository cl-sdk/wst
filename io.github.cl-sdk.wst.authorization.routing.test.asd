(asdf:defsystem #:io.github.cl-sdk.wst.authorization.routing.test
  :description "Tests for io.github.cl-sdk.wst.authorization.routing."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.authorization
               #:io.github.cl-sdk.wst.authorization.routing
               #:io.github.cl-sdk.wst.routing)
  :pathname "t"
  :serial t
  :components ((:file "authorization-routing-tests")))
