(asdf:defsystem #:wst.request-content.routing
  :description "HTTP middleware adapter to parse request bodies with wst.request-content."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:wst.request-content
               #:wst.routing)
  :pathname "request-content"
  :serial t
  :components ((:file "routing")))
