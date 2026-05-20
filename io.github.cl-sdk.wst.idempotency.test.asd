(asdf:defsystem #:io.github.cl-sdk.wst.idempotency.test
  :description "Tests for io.github.cl-sdk.wst.idempotency."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.idempotency.store
               #:io.github.cl-sdk.wst.idempotency.memory-store
               #:io.github.cl-sdk.wst.idempotency)
  :pathname "t"
  :serial t
  :components ((:file "idempotency-tests")))

