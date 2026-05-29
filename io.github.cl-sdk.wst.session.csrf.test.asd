(asdf:defsystem #:io.github.cl-sdk.wst.session.csrf.test
  :description "Tests for io.github.cl-sdk.wst.session.csrf."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.session.csrf)
  :pathname "t"
  :serial t
  :components ((:file "session-csrf-tests")))
