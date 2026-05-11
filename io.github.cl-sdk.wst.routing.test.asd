(asdf:defsystem #:io.github.cl-sdk.wst.routing.test
  :description "Tests for io.github.cl-sdk.wst.routing."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:cl-hash-util
               #:flexi-streams
               #:io.github.cl-sdk.wst.request-content
               #:io.github.cl-sdk.wst.cookies
               #:io.github.cl-sdk.wst.routing)
  :pathname "t"
  :serial t
  :components ((:file "routing-tests")))
