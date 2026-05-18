(asdf:defsystem #:io.github.cl-sdk.wst.rate-limit.store
  :description "Storage backend protocol for wst.rate-limit tracking (fetch/save/delete window)."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on ()
  :pathname "rate-limit"
  :serial t
  :components ((:file "store")))
