(asdf:defsystem #:io.github.cl-sdk.wst.idempotency.memory-store
  :description "Built-in in-memory storage backend for idempotency keys."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.idempotency.store
               #:io.github.cl-sdk.wst.idempotency)
  :pathname "idempotency"
  :serial t
  :components ((:file "memory-store")))
