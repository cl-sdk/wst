(asdf:defsystem #:io.github.cl-sdk.wst.test.support
  :description "Shared package and test helpers for wst FiveAM test systems."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:fiveam
               #:io.github.cl-sdk.wst.routing)
  :pathname "t"
  :serial t
  :components ((:file "package")))
