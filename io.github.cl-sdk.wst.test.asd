(asdf:defsystem #:io.github.cl-sdk.wst.test
  :description "FiveAM test suite for io.github.cl-sdk.wst.routing, io.github.cl-sdk.wst.routing.dsl, io.github.cl-sdk.wst.routing.woo, and io.github.cl-sdk.wst.routing.response.dsl."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:alexandria
               #:str
               #:serapeum
               #:cl-hash-util
               #:fiveam
               #:io.github.cl-sdk.wst.http
               #:io.github.cl-sdk.wst.request-content
               #:io.github.cl-sdk.wst.request-content.routing
               #:io.github.cl-sdk.wst.circuit-breaker
               #:io.github.cl-sdk.wst.circuit-breaker.routing
               #:io.github.cl-sdk.wst.cookies
               #:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.dsl
               #:io.github.cl-sdk.wst.routing.woo
               #:io.github.cl-sdk.wst.routing.response.dsl
               #:io.github.cl-sdk.wst.rate-limit.store
               #:io.github.cl-sdk.wst.rate-limit.memory-store
               #:io.github.cl-sdk.wst.rate-limit
               #:io.github.cl-sdk.wst.cors)
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "routing-tests")
               (:file "routing-dsl-tests")
               (:file "circuit-breaker-tests")
               (:file "rate-limit-tests")
               (:file "cors-tests")
               (:file "routing-woo-tests")
               (:file "response-dsl-tests")
               (:file "request-content-tests")
               (:file "request-content-routing-tests")))
