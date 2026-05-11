(asdf:defsystem #:io.github.cl-sdk.wst.routing.woo.test
  :description "Tests for io.github.cl-sdk.wst.routing.woo."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:cl-hash-util
               #:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.woo)
  :pathname "t"
  :serial t
  :components ((:file "routing-woo-tests")))
