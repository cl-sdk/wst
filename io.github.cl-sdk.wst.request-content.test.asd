(asdf:defsystem #:io.github.cl-sdk.wst.request-content.test
  :description "Tests for io.github.cl-sdk.wst.request-content."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.request-content)
  :pathname "t"
  :serial t
  :components ((:file "request-content-tests")))
