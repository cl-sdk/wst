(asdf:defsystem #:io.github.cl-sdk.wst.feature-flag.routing
  :description "Routing adapter for request-scoped feature-flag evaluation context."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.feature-flag
               #:io.github.cl-sdk.wst.routing)
  :pathname "feature-flag"
  :serial t
  :components ((:file "routing")))
