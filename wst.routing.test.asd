(asdf:defsystem #:wst.routing.test
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util
               #:fiveam
               #:wst.routing
               #:wst.routing.dsl
               #:wst.routing.woo)
  :pathname "routing"
  :serial t
  :components ((:file "test")))
