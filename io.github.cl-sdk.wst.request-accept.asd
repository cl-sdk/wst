(asdf:defsystem #:io.github.cl-sdk.wst.request-accept
  :description "Stateless Accept header parsing utilities for wst web applications."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:str #:serapeum)
  :pathname "request-accept"
  :serial t
  :components ((:file "package")))
