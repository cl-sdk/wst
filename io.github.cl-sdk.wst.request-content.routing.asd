(asdf:defsystem #:io.github.cl-sdk.wst.request-content.routing
  :description "HTTP middleware adapter to parse request bodies with wst.request-content."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:io.github.cl-sdk.wst.request-content
               #:io.github.cl-sdk.wst.routing)
  :pathname "request-content"
  :serial t
  :components ((:file "routing")))
