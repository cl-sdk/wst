(asdf:defsystem #:wst.request-content
  :description "Stateless Content-Type header parsing and content dispatch for wst web applications."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:str
               #:flexi-streams)
  :pathname "request-content"
  :serial t
  :components ((:file "package")))
