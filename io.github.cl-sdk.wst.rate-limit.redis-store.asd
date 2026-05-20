(asdf:defsystem #:io.github.cl-sdk.wst.rate-limit.redis-store
  :description "Redis-backed storage backend for wst.rate-limit."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:io.github.cl-sdk.wst.rate-limit
	       #:cl-redis)
  :pathname "rate-limit"
  :serial t
  :components ((:file "redis-store")))
