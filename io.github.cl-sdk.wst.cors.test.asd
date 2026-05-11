(asdf:defsystem #:io.github.cl-sdk.wst.cors.test
  :description "Tests for io.github.cl-sdk.wst.cors."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.cors)
  :pathname "t"
  :serial t
  :components ((:file "cors-tests")))
