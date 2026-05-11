(asdf:defsystem #:io.github.cl-sdk.wst.rate-limit.test
  :description "Tests for io.github.cl-sdk.wst.rate-limit and store implementations."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.rate-limit.store
               #:io.github.cl-sdk.wst.rate-limit.memory-store
               #:io.github.cl-sdk.wst.rate-limit)
  :pathname "t"
  :serial t
  :components ((:file "rate-limit-tests")))
