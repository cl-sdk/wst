(asdf:defsystem #:io.github.cl-sdk.wst.rate-limit
  :description "Fixed-window rate limiting for wst web applications."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:io.github.cl-sdk.wst.rate-limit.store
               #:io.github.cl-sdk.wst.rate-limit.memory-store)
  :pathname "throttle"
  :serial t
  :components ((:file "rate-limit")))
