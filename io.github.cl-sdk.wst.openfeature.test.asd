(asdf:defsystem #:io.github.cl-sdk.wst.openfeature.test
  :description "Tests for io.github.cl-sdk.wst.openfeature."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.openfeature)
  :pathname "t"
  :serial t
  :components ((:file "openfeature-tests")))
