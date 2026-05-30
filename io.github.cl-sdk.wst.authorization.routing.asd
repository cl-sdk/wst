(asdf:defsystem #:io.github.cl-sdk.wst.authorization.routing
  :description "Routing adapter for request-scoped authorization checks."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.authorization
               #:io.github.cl-sdk.wst.routing)
  :pathname "authorization"
  :serial t
  :components ((:file "routing")))
