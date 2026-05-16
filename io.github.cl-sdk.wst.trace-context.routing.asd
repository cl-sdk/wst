(asdf:defsystem #:io.github.cl-sdk.wst.trace-context.routing
  :description "W3C Trace Context routing adapter: injects trace context into wst request data."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.trace-context
               #:io.github.cl-sdk.wst.routing)
  :pathname "trace-context"
  :serial t
  :components ((:file "routing")))
