(asdf:defsystem #:wst.routing.test
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util
               #:fiveam
               #:wst.routing
               #:wst.routing.dsl)
  :pathname "routing"
  :serial t
  :components ((:file "test")))
