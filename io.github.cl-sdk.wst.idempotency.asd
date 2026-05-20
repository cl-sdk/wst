(asdf:defsystem #:io.github.cl-sdk.wst.idempotency
  :description "Core idempotency-key lifecycle and replay policy."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.idempotency.store
               #:io.github.cl-sdk.wst.idempotency.memory-store)
  :pathname "idempotency"
  :serial t
  :components ((:file "package")))
