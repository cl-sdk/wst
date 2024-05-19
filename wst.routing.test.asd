(asdf:defsystem #:wst.routing.test
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util
               #:fiveam
               #:wst.routing)
  :pathname "routing"
  :serial t
  :components ((:file "test")))
