(asdf:defsystem #:wst.routing.response.dsl
  :author "Bruno Dias"
  :serial t
  :depends-on (#:wst.routing)
  :components ((:module "routing"
                :components ((:file "response-dsl")))))
