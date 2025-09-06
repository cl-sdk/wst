(asdf:defsystem #:wst.routing.test
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:str
               #:serapeum
               #:cl-hash-util
               #:fiveam
               #:wst.http
               #:wst.routing
               #:wst.routing.dsl
               #:wst.routing.woo
               #:wst.routing.response.dsl)
  :pathname "t"
  :serial t
  :components ((:file "package")))
