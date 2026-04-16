(asdf:defsystem #:wst.routing.static
  :description "Macro helper for registering GET routes that serve static files."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :serial t
  :pathname "static"
  :depends-on (#:wst.routing)
  :components ((:file "package")))
