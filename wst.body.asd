(asdf:defsystem #:wst.body
  :description "Stateless MIME-type dispatch and body parsing for wst web applications."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:str
               #:flexi-streams
               #:com.inuoe.jzon)
  :pathname "body"
  :serial t
  :components ((:file "package")))
