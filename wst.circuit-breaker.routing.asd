(asdf:defsystem #:wst.circuit-breaker.routing
  :description "HTTP adapter bridging wst.circuit-breaker and wst.routing middleware."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:wst.circuit-breaker
               #:wst.routing)
  :pathname "circuit-breaker"
  :serial t
  :components ((:file "package")
               (:file "routing")))
