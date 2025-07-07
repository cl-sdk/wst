(asdf:defsystem #:wst.routing
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
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
               (:file "routes")
               (:file "static")))
