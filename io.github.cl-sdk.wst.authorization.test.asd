(asdf:defsystem #:io.github.cl-sdk.wst.authorization.test
  :description "Tests for io.github.cl-sdk.wst.authorization."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.authorization)
  :pathname "t"
  :serial t
  :components ((:file "authorization-tests")))
