(asdf:defsystem #:wst.rate-limit.memory-store
  :description "Built-in in-memory storage backend for wst.rate-limit."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:wst.rate-limit.store)
  :pathname "throttle"
  :serial t
  :components ((:file "memory-store")))
