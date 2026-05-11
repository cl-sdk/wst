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

(asdf:defsystem #:io.github.cl-sdk.wst.routing.test
  :description "Tests for io.github.cl-sdk.wst.routing."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:cl-hash-util
               #:flexi-streams
               #:io.github.cl-sdk.wst.request-content
               #:io.github.cl-sdk.wst.cookies
               #:io.github.cl-sdk.wst.routing)
  :pathname "t"
  :serial t
  :components ((:file "routing-tests")))

(asdf:defsystem #:io.github.cl-sdk.wst.routing.dsl.test
  :description "Tests for io.github.cl-sdk.wst.routing.dsl."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.circuit-breaker.routing
               #:io.github.cl-sdk.wst.rate-limit
               #:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.dsl)
  :pathname "t"
  :serial t
  :components ((:file "routing-dsl-tests")))

(asdf:defsystem #:io.github.cl-sdk.wst.circuit-breaker.test
  :description "Tests for io.github.cl-sdk.wst.circuit-breaker."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.circuit-breaker)
  :pathname "t"
  :serial t
  :components ((:file "circuit-breaker-tests")))

(asdf:defsystem #:io.github.cl-sdk.wst.rate-limit.test
  :description "Tests for io.github.cl-sdk.wst.rate-limit and store implementations."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.rate-limit.store
               #:io.github.cl-sdk.wst.rate-limit.memory-store
               #:io.github.cl-sdk.wst.rate-limit)
  :pathname "t"
  :serial t
  :components ((:file "rate-limit-tests")))

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

(asdf:defsystem #:io.github.cl-sdk.wst.routing.woo.test
  :description "Tests for io.github.cl-sdk.wst.routing.woo."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:cl-hash-util
               #:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.woo)
  :pathname "t"
  :serial t
  :components ((:file "routing-woo-tests")))

(asdf:defsystem #:io.github.cl-sdk.wst.routing.response.dsl.test
  :description "Tests for io.github.cl-sdk.wst.routing.response.dsl."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.http
               #:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.response.dsl)
  :pathname "t"
  :serial t
  :components ((:file "response-dsl-tests")))

(asdf:defsystem #:io.github.cl-sdk.wst.request-content.test
  :description "Tests for io.github.cl-sdk.wst.request-content."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.request-content)
  :pathname "t"
  :serial t
  :components ((:file "request-content-tests")))

(asdf:defsystem #:io.github.cl-sdk.wst.request-content.routing.test
  :description "Tests for io.github.cl-sdk.wst.request-content.routing."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.test.support
               #:io.github.cl-sdk.wst.request-content
               #:io.github.cl-sdk.wst.request-content.routing
               #:io.github.cl-sdk.wst.routing
               #:io.github.cl-sdk.wst.routing.dsl)
  :pathname "t"
  :serial t
  :components ((:file "request-content-routing-tests")))

(asdf:defsystem #:io.github.cl-sdk.wst.test
  :description "Umbrella FiveAM test system for all io.github.cl-sdk.wst test subsystems."
  :author "Bruno Dias"
  :license "Unlicense"
  :version "0.1.0"
  :depends-on (#:io.github.cl-sdk.wst.routing.test
               #:io.github.cl-sdk.wst.routing.dsl.test
               #:io.github.cl-sdk.wst.circuit-breaker.test
               #:io.github.cl-sdk.wst.rate-limit.test
               #:io.github.cl-sdk.wst.cors.test
               #:io.github.cl-sdk.wst.routing.woo.test
               #:io.github.cl-sdk.wst.routing.response.dsl.test
               #:io.github.cl-sdk.wst.request-content.test
               #:io.github.cl-sdk.wst.request-content.routing.test))
