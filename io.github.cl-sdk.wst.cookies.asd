(asdf:defsystem #:io.github.cl-sdk.wst.cookies
  :description "HTTP cookie parsing helpers backed by cl-cookie."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:cl-cookie)
  :pathname "cookies"
  :serial t
  :components ((:file "package")))
