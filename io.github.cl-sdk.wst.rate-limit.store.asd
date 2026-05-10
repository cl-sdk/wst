(asdf:defsystem #:io.github.cl-sdk.wst.rate-limit.store
  :description "Storage backend protocol for wst.rate-limit tracking (fetch/save/delete window)."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on ()
  :pathname "throttle"
  :serial t
  :components ((:file "store")))
