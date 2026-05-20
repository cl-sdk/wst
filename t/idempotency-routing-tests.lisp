(in-package :io.github.cl-sdk.wst.test)

(5am:def-suite wst.idempotency.routing.suite
  :description "Tests for wst.idempotency.routing middleware.")

(5am:in-suite wst.idempotency.routing.suite)

(def-route-testing idempotency-middleware-requires-key-by-default ()
  (let ((calls 0)
        (middleware (io.github.cl-sdk.wst.idempotency.routing:idempotency-key)))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,(getf middleware :before)
       :after ,(getf middleware :after)
       :route (:route :POST create "/" (lambda (request response)
                                          (declare (ignore request))
                                          (incf calls)
                                          (io.github.cl-sdk.wst.routing:ok-response t response :content "ok")
                                          response))))
    (let ((response (io.github.cl-sdk.wst.routing:dispatch-route
                     (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :POST))))
      (5am:is (= 0 calls))
      (5am:is (= 400 (io.github.cl-sdk.wst.routing:response-status response))))))

(def-route-testing idempotency-middleware-replays-completed-response ()
  (let ((calls 0)
        (middleware (io.github.cl-sdk.wst.idempotency.routing:idempotency-key)))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,(getf middleware :before)
       :after ,(getf middleware :after)
       :route (:route :POST create "/" (lambda (request response)
                                          (declare (ignore request))
                                          (incf calls)
                                          (io.github.cl-sdk.wst.routing:ok-response t response :content "created")
                                          response))))
    (let* ((headers (cl-hash-util:hash ("idempotency-key" "abc-1")))
           (first (io.github.cl-sdk.wst.routing:dispatch-route
                   (io.github.cl-sdk.wst.routing:make-request
                    :uri "/" :method :POST :headers headers)))
           (second (io.github.cl-sdk.wst.routing:dispatch-route
                    (io.github.cl-sdk.wst.routing:make-request
                     :uri "/" :method :POST :headers headers))))
      (5am:is (= 1 calls))
      (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status first)))
      (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status second)))
      (5am:is (string= "created" (io.github.cl-sdk.wst.routing:response-content second)))
      (5am:is (string= "true" (getf (io.github.cl-sdk.wst.routing:response-headers second)
                                    :idempotency-replayed)))))

(def-route-testing idempotency-middleware-rejects-conflicting-payload ()
  (let ((calls 0)
        (middleware (io.github.cl-sdk.wst.idempotency.routing:idempotency-key)))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,(getf middleware :before)
       :after ,(getf middleware :after)
       :route (:route :POST create "/" (lambda (request response)
                                          (declare (ignore request))
                                          (incf calls)
                                          (io.github.cl-sdk.wst.routing:ok-response t response :content "created")
                                          response))))
    (let* ((headers (cl-hash-util:hash ("Idempotency-Key" "abc-2")))
           (first (io.github.cl-sdk.wst.routing:dispatch-route
                   (io.github.cl-sdk.wst.routing:make-request
                    :uri "/" :method :POST :headers headers :content "name=alice")))
           (second (io.github.cl-sdk.wst.routing:dispatch-route
                    (io.github.cl-sdk.wst.routing:make-request
                     :uri "/" :method :POST :headers headers :content "name=bob"))))
      (5am:is (= 1 calls))
      (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status first)))
      (5am:is (= 409 (io.github.cl-sdk.wst.routing:response-status second))))))

(5am:def-test idempotency-middleware-blocks-while-in-progress ()
  (let* ((middleware (io.github.cl-sdk.wst.idempotency.routing:idempotency-key))
         (before (getf middleware :before))
         (request-a (io.github.cl-sdk.wst.routing:make-request
                     :uri "/payments"
                     :method :POST
                     :headers (cl-hash-util:hash ("Idempotency-Key" "same"))))
         (request-b (io.github.cl-sdk.wst.routing:make-request
                     :uri "/payments"
                     :method :POST
                     :headers (cl-hash-util:hash ("Idempotency-Key" "same"))))
         (response-a (io.github.cl-sdk.wst.routing:make-response))
         (response-b (io.github.cl-sdk.wst.routing:make-response)))
    (destructuring-bind (control-a . resulting-a)
        (funcall before request-a response-a)
      (declare (ignore resulting-a))
      (5am:is (eq :continue control-a)))
    (destructuring-bind (control-b . resulting-b)
        (funcall before request-b response-b)
      (declare (ignore resulting-b))
      (5am:is (eq :halt control-b))
      (5am:is (= 409 (io.github.cl-sdk.wst.routing:response-status response-b))))))
