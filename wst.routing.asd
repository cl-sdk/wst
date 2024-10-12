(asdf:defsystem #:wst.routing
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util
               #:flexi-streams
               #:serapeum
               #:com.inuoe.jzon)
  :pathname "routing"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "responses")
               (:file "match")
               (:file "routes")
               (:file "static")))
