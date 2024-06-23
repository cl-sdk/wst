(asdf:defsystem #:wst.routing
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util
               #:flexi-streams
	       #:serapeum
	       #:com.inuoe.jzon
               #:log4cl)
  :pathname "routing"
  :serial t
  :components ((:file "package")))
