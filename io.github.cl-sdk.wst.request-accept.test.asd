(asdf:defsystem #:io.github.cl-sdk.wst.request-accept.test
  :description "Tests for io.github.cl-sdk.wst.request-accept."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:cl-hash-util
               #:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.request-accept)
  :pathname "t"
  :serial t
  :components ((:file "request-accept-tests")))
