(asdf:defsystem #:wst.routing.dsl
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util
               #:wst.routing)
  :pathname "routing"
  :serial t
  :components ((:file "dsl")))
