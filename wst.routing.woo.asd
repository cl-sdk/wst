(asdf:defsystem #:wst.routing.woo
  :description "Adapter that bridges the Woo HTTP server environment to wst.routing request/response objects."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:wst.routing)
  :pathname "adapters"
  :serial t
  :components ((:file "woo")))
