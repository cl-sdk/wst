(asdf:defsystem #:wst.rate-limit
  :description "Fixed-window rate limiting for wst web applications."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:wst.rate-limit.store)
  :pathname "throttle"
  :serial t
  :components ((:file "rate-limit")))
