(asdf:defsystem #:io.github.cl-sdk.wst.circuit-breaker.test
  :description "Tests for io.github.cl-sdk.wst.circuit-breaker."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.circuit-breaker)
  :pathname "t"
  :serial t
  :components ((:file "circuit-breaker-tests")))
