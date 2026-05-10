(asdf:defsystem #:io.github.cl-sdk.wst.routing.woo
  :description "Adapter that bridges the Woo HTTP server environment to wst.routing request/response objects."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.routing)
  :pathname "adapters"
  :serial t
  :components ((:file "woo")))
