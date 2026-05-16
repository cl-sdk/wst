(asdf:defsystem #:io.github.cl-sdk.wst.feature-flag.test
  :description "Tests for io.github.cl-sdk.wst.feature-flag."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.feature-flag)
  :pathname "t"
  :serial t
  :components ((:file "feature-flag-tests")))
