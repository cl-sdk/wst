(asdf:defsystem #:io.github.cl-sdk.wst.idempotency.routing
  :description "Routing middleware adapter for idempotency keys."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:io.github.cl-sdk.wst.idempotency
               #:io.github.cl-sdk.wst.routing)
  :pathname "idempotency"
  :serial t
  :components ((:file "routing")))

