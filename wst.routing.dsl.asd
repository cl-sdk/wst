(asdf:defsystem #:wst.routing.dsl
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util
               #:wst.routing)
  :pathname "routing"
  :serial t
  :components ((:file "dsl")))
