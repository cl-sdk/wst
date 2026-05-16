(asdf:defsystem #:io.github.cl-sdk.wst.feature-flag.routing.test
  :description "Tests for io.github.cl-sdk.wst.feature-flag.routing."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.feature-flag
               #:io.github.cl-sdk.wst.feature-flag.routing
               #:io.github.cl-sdk.wst.routing)
  :pathname "t"
  :serial t
  :components ((:file "feature-flag-routing-tests")))
