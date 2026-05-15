(asdf:defsystem #:io.github.cl-sdk.wst.trace-context.test
  :description "Tests for io.github.cl-sdk.wst.trace-context."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.trace-context)
  :pathname "t"
  :serial t
  :components ((:file "trace-context-tests")))
