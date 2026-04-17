(asdf:defsystem #:wst.routing
  :description "Core route registration, matching, and dispatch for wst web applications."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util)
  :pathname "routing"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "responses")
               (:file "routes")))
