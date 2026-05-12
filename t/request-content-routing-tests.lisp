(in-package :io.github.cl-sdk.wst.test)

(5am:def-suite wst.request-content.routing.suite
  :description "Tests for wst.request-content.routing middleware.")

(5am:in-suite wst.request-content.routing.suite)

(def-route-testing request-content-middleware-parses-form-urlencoded-content ()
  (let ((middleware (io.github.cl-sdk.wst.request-content.routing:parse-request-content))
        (handler (lambda (request response)
                   (io.github.cl-sdk.wst.routing:ok-response t response
                                            :content (write-to-string
                                                      (io.github.cl-sdk.wst.routing:request-content request))))))
    (io.github.cl-sdk.wst.routing:condition-handler #'io.github.cl-sdk.wst.routing:development-condition-handler)
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,middleware
       :route (:route :POST parse-content "/" ,handler)))
    (let ((response (io.github.cl-sdk.wst.routing:dispatch-route
                     (io.github.cl-sdk.wst.routing:make-request
                      :uri "/"
                      :method :POST
                      :content-type "application/x-www-form-urlencoded"
                      :content "name=alice+smith&email=alice%40test.dev"))))
      (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status response)))
      (5am:is (equal (write-to-string '(("name" . "alice smith") ("email" . "alice@test.dev")))
                     (io.github.cl-sdk.wst.routing:response-content response))))))

(defmethod io.github.cl-sdk.wst.request-content:parse-content
    ((type (eql :|application/x-fail|)) content &optional (encoding :us-ascii))
  (declare (ignore type content encoding))
  (error "boom"))

(def-route-testing request-content-middleware-halts-with-400-when-parsing-fails ()
  (let* ((calls 0)
         (middleware (io.github.cl-sdk.wst.request-content.routing:parse-request-content))
         (handler (lambda (request response)
                    (declare (ignore request response))
                    (incf calls))))
    (io.github.cl-sdk.wst.routing:condition-handler #'io.github.cl-sdk.wst.routing:development-condition-handler)
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,middleware
       :route (:route :POST fail-content "/" ,handler)))
    (let ((response (io.github.cl-sdk.wst.routing:dispatch-route
                     (io.github.cl-sdk.wst.routing:make-request
                      :uri "/"
                      :method :POST
                      :content-type "application/x-fail"
                      :content "x"))))
      (5am:is (= 0 calls))
      (5am:is (= 400 (io.github.cl-sdk.wst.routing:response-status response))))))

(def-route-testing request-content-middleware-uses-configured-default-content-type ()
  (let ((middleware (io.github.cl-sdk.wst.request-content.routing:parse-request-content
                     :default-content-type "application/x-www-form-urlencoded"))
        (handler (lambda (request response)
                   (io.github.cl-sdk.wst.routing:ok-response t response
                                                             :content (write-to-string (io.github.cl-sdk.wst.routing:request-content request))))))
    (io.github.cl-sdk.wst.routing:condition-handler #'io.github.cl-sdk.wst.routing:development-condition-handler)
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,middleware
       :route (:route :POST default-content-type "/" ,handler)))
    (let ((response (io.github.cl-sdk.wst.routing:dispatch-route
                     (io.github.cl-sdk.wst.routing:make-request
                      :uri "/"
                      :method :POST
                      :content-type ""
                      :content "name=alice+smith"))))
      (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status response)))
      (5am:is (equal (write-to-string '(("name" . "alice smith")))
                     (io.github.cl-sdk.wst.routing:response-content response))))))
