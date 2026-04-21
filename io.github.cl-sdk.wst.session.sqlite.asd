(asdf:defsystem #:io.github.cl-sdk.wst.session.sqlite
  :description "SQLite-backed implementation of the wst.session protocol."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:io.github.cl-sdk.wst.session
               #:sqlite)
  :pathname "session"
  :serial t
  :components ((:file "sqlite-store")))
