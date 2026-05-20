(asdf:defsystem #:io.github.cl-sdk.wst.rate-limit.redis-store.test
  :description "Redis-backed rate limit storage backend."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:fiveam
               #:io.github.cl-sdk.wst.rate-limit.redis-store)
  :pathname "t"
  :serial t
  :components ((:file "rate-limit-redis-store-tests")))
