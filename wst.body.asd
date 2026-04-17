(asdf:defsystem #:wst.request-accept-content
  :description "Stateless Accept-header parsing and content dispatch for wst web applications."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:str
               #:flexi-streams)
  :pathname "body"
  :serial t
  :components ((:file "package")))
