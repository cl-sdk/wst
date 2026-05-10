(asdf:defsystem #:io.github.cl-sdk.wst.request-content
  :description "Stateless Content-Type header parsing and content dispatch for wst web applications."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:str
               #:flexi-streams)
  :pathname "request-content"
  :serial t
  :components ((:file "package")))
