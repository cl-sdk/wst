(asdf:defsystem #:io.github.cl-sdk.wst.routing.dsl
  :description "Declarative DSL for composing routes with middleware (group/resource/wrap/any-route)."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:str
               #:cl-hash-util
               #:io.github.cl-sdk.wst.routing)
  :pathname "routing"
  :serial t
  :components ((:file "dsl")))
