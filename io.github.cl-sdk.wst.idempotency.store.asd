(asdf:defsystem #:io.github.cl-sdk.wst.idempotency.store
  :description "Storage backend protocol for idempotency keys."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on ()
  :pathname "idempotency"
  :serial t
  :components ((:file "store")))
