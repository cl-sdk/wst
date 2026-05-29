(asdf:defsystem #:io.github.cl-sdk.wst.flash.test
  :description "Tests for io.github.cl-sdk.wst.flash."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.flash)
  :pathname "t"
  :serial t
  :components ((:file "flash-tests")))
