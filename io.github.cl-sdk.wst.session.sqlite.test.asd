(asdf:defsystem #:io.github.cl-sdk.wst.session.sqlite.test
  :description "SQLite-backed implementation of the wst.session protocol."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.0.1"
  :depends-on (#:sqlite
               #:io.github.cl-sdk.wst.session
               #:io.github.cl-sdk.wst.session.sqlite)
  :pathname "t"
  :serial t
  :components ((:file "session-sqlite-store-tests")))
