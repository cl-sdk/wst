(asdf:defsystem #:io.github.cl-sdk.wst.routing.response.dsl
  :description "Fluent helpers for setting response status, headers, and content type (text/html/json)."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :serial t
  :depends-on (#:io.github.cl-sdk.wst.routing)
  :pathname "routing"
  :components ((:file "response-dsl")))
