(in-package :wst.routing.test)

(5am:def-suite wst.request-content.routing.suite
  :description "Tests for wst.request-content.routing middleware.")

(5am:in-suite wst.request-content.routing.suite)

(def-route-testing request-content-middleware-parses-form-urlencoded-content ()
  (let ((middleware (wst.request-content.routing:parse-request-content))
        (handler (lambda (request response)
                   (wst.routing:ok-response t response
                                            :content (write-to-string
                                                      (wst.routing:request-content request))))))
    (wst.routing:condition-handler #'wst.routing:development-condition-handler)
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,middleware
       :route (wst.routing.dsl:route :POST parse-content "/" ,handler)))
    (let ((response (wst.routing:dispatch-route
                     (wst.routing:make-request
                      :uri "/"
                      :method :POST
                      :content-type "application/x-www-form-urlencoded"
                      :content "name=alice+smith&email=alice%40test.dev"))))
      (5am:is (= 200 (wst.routing:response-status response)))
      (5am:is (equal (write-to-string '(("name" . "alice smith") ("email" . "alice@test.dev")))
                     (wst.routing:response-content response))))))

(defmethod wst.request-content:parse-content
    ((type (eql :|application/x-fail|)) content &optional (encoding :us-ascii))
  (declare (ignore type content encoding))
  (error "boom"))

(def-route-testing request-content-middleware-halts-with-400-when-parsing-fails ()
  (let* ((calls 0)
         (middleware (wst.request-content.routing:parse-request-content))
         (handler (lambda (request response)
                    (declare (ignore request response))
                    (incf calls))))
    (wst.routing:condition-handler #'wst.routing:development-condition-handler)
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,middleware
       :route (wst.routing.dsl:route :POST fail-content "/" ,handler)))
    (let ((response (wst.routing:dispatch-route
                     (wst.routing:make-request
                      :uri "/"
                      :method :POST
                      :content-type "application/x-fail"
                      :content "x"))))
      (5am:is (= 0 calls))
      (5am:is (= 400 (wst.routing:response-status response))))))

(def-route-testing request-content-middleware-uses-configured-default-content-type ()
  (let ((middleware (wst.request-content.routing:parse-request-content
                     :default-content-type "application/x-www-form-urlencoded"))
        (handler (lambda (request response)
                   (wst.routing:ok-response t response
                                            :content (write-to-string (wst.routing:request-content request))))))
    (wst.routing:condition-handler #'wst.routing:development-condition-handler)
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,middleware
       :route (wst.routing.dsl:route :POST default-content-type "/" ,handler)))
    (let ((response (wst.routing:dispatch-route
                     (wst.routing:make-request
                      :uri "/"
                      :method :POST
                      :content-type ""
                      :content "name=alice+smith"))))
      (5am:is (= 200 (wst.routing:response-status response)))
      (5am:is (equal (write-to-string '(("name" . "alice smith")))
                     (wst.routing:response-content response))))))
