(asdf:defsystem #:io.github.cl-sdk.wst.openfeature.routing
  :description "Routing adapter for request-scoped OpenFeature evaluation context."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.openfeature
               #:io.github.cl-sdk.wst.routing)
  :pathname "openfeature"
  :serial t
  :components ((:file "routing")))
