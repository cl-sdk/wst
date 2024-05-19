(asdf:defsystem #:wst.routing
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util
               #:serapeum
               #:log4cl)
  :pathname "routing"
  :serial t
  :components ((:file "package")))
