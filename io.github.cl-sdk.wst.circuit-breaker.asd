(asdf:defsystem #:io.github.cl-sdk.wst.circuit-breaker
  :description "Pure circuit breaker state machine with no HTTP dependencies."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on ()
  :pathname "circuit-breaker"
  :serial t
  :components ((:file "package")))
