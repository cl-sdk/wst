(asdf:defsystem #:wst.throttle.store
  :description "Storage backend protocol for wst.throttle rate-limit tracking (fetch/save/delete window)."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on ()
  :pathname "throttle"
  :serial t
  :components ((:file "store")))
