(in-package :io.github.cl-sdk.wst.test)

;;;
;;; wst.routing.dsl suite
;;;

(5am:def-suite wst.routing.dsl.suite
  :description "Tests for the wst.routing.dsl package.")

(5am:in-suite wst.routing.dsl.suite)

(defun route-responder (request response)
  (declare (ignore request response))
  (5am:is-true t))

(def-route-testing build-a-simple-route-using-the-dsl ()
  (io.github.cl-sdk.wst.routing.dsl:build-webserver
   `(:route :GET index "/" route-responder))
  (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET)))

(def-route-testing build-with-just-route-is-the-same-as-plain-route ()
  (let* ((count 0)
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :route (:route :GET index "/" ,handler)))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (5am:is (= 1 count))))

(def-route-testing build-route-with-just-before-middleware ()
  (let* ((count 0)
         (before-action (lambda (req res)
                          (declare (ignore req res))
                          (setf count (1+ count))
                          (cons :continue res)))
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,before-action
       :route (:route :GET index "/" ,handler)))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-route-with-just-after-middleware ()
  (let* ((count 0)
         (after-action (lambda (req res)
                         (declare (ignore req res))
                         (setf count (1+ count))))
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :route (:route :GET index "/" ,handler)
       :after ,after-action))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-route-with-before-and-after-middleware ()
  (let* ((count 0)
         (middleware (lambda (req res)
                       (declare (ignore req res))
                       (setf count (1+ count))
                       (cons :continue res))))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,middleware
       :route (:route :GET index "/" ,middleware)
       :after ,middleware))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (5am:is (= 3 count))))


;;;
;;; circuit breaker HTTP adapter (wst.routing.dsl integration) suite
;;;

(5am:in-suite wst.routing.dsl.suite)

(def-route-testing circuit-breaker-opens-after-failure-threshold ()
  (let* ((now 0)
         (count 0)
         (cb (io.github.cl-sdk.wst.circuit-breaker.routing:circuit-breaker
              :failure-threshold 2
              :recovery-timeout 30
              :clock (lambda () now)))
         (before (getf cb :before))
         (after (getf cb :after))
         (handler (lambda (req res)
                    (declare (ignore req))
                    (incf count)
                    (setf (io.github.cl-sdk.wst.routing:response-status res) 500)
                    res)))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,before
       :after ,after
       :route (:route :GET index "/" ,handler)))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (let ((blocked-a (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET)))
          (blocked-b (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))))
      (5am:is (= 2 count))
      (5am:is (= 503 (io.github.cl-sdk.wst.routing:response-status blocked-a)))
      (5am:is (string-equal "service unavailable" (io.github.cl-sdk.wst.routing:response-content blocked-a)))
      (5am:is (= 503 (io.github.cl-sdk.wst.routing:response-status blocked-b)))
      (5am:is (string-equal "service unavailable" (io.github.cl-sdk.wst.routing:response-content blocked-b))))))

(def-route-testing circuit-breaker-half-open-recovery-closes-on-success ()
  (let* ((now 0)
         (count 0)
         (should-fail t)
         (cb (io.github.cl-sdk.wst.circuit-breaker.routing:circuit-breaker
              :failure-threshold 1
              :recovery-timeout 10
              :clock (lambda () now)))
         (before (getf cb :before))
         (after (getf cb :after))
         (handler (lambda (req res)
                    (declare (ignore req))
                    (incf count)
                    (setf (io.github.cl-sdk.wst.routing:response-status res) (if should-fail 500 200))
                    res)))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,before
       :after ,after
       :route (:route :GET index "/" ,handler)))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (let ((blocked (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))))
      (5am:is (= 503 (io.github.cl-sdk.wst.routing:response-status blocked))))
    (setf now 11
          should-fail nil)
    (let ((half-open-success (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET)))
          (after-closed (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))))
      (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status half-open-success)))
      (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status after-closed)))
      (5am:is (= 3 count)))
    (setf should-fail t)
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (let ((blocked-again (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'index (io.github.cl-sdk.wst.routing:make-request :method :GET))))
      (5am:is (= 503 (io.github.cl-sdk.wst.routing:response-status blocked-again)))
      (5am:is (= 4 count)))))

(def-route-testing build-group-of-routes ()
  (let* ((count 0)
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:group
       (:route :GET route-a "/a" ,handler)
       (:route :GET route-b "/b" ,handler)))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'route-a (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'route-b (io.github.cl-sdk.wst.routing:make-request :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-resource-routes-with-prefix ()
  (let* ((count 0)
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:resource "/base"
                                (:route :GET route-a "/a" ,handler)
                                (:route :GET route-b ,handler)))
    (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/base/a" :method :GET))
    (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/base" :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-any-route-catches-all-uris ()
  (let* ((count 0)
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:any-route :GET ,handler))
    (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/anything" :method :GET))
    (5am:is (= 1 count))))

(def-route-testing build-route-with-custom-metadata ()
  (let* ((handler (lambda (req res) (declare (ignore req)) res)))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:group
       (:route :GET route-a "/" ,handler :custom :ok)
       (:resource "/a"
                                 (:route :GET route-b ,handler :custom :ok))))
    (5am:is-true (equal :ok (car (io.github.cl-sdk.wst.routing::route-custom
                                  (io.github.cl-sdk.wst.routing:find-route-by-name 'route-a)))))
    (5am:is-true (equal :ok (car (io.github.cl-sdk.wst.routing::route-custom
                                  (io.github.cl-sdk.wst.routing:find-route-by-name 'route-b)))))))

(def-route-testing rate-limit-throttles-after-limit-is-reached ()
  ;; wst.rate-limit:rate-limit is a pure rate-limiter that knows nothing about
  ;; requests or responses. Here we compose it into a DSL before-middleware
  ;; by hand, using the three values it returns.
  (let* ((limiter (io.github.cl-sdk.wst.rate-limit:rate-limit :max-requests 1 :window-seconds 60))
         (middleware (lambda (request response)
                       (declare (ignorable request))
                       (multiple-value-bind (allowed-p retry-after)
                           (funcall limiter :global)
                         (if allowed-p
                             (cons :continue response)
                             (cons :halt
                                   (io.github.cl-sdk.wst.routing:too-many-requests-response
                                    t response
                                    :headers (list :retry-after
                                                   (format nil "~a" retry-after))))))))
         (handler (lambda (req res)
                    (declare (ignore req))
                    (io.github.cl-sdk.wst.routing:ok-response t res :content "ok")
                    res)))
    (io.github.cl-sdk.wst.routing.dsl:build-webserver
     `(:wrap
       :before ,middleware
       :route (:route :GET throttled "/" ,handler)))
    (let ((first  (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET)))
          (second (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))
      (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status first)))
      (5am:is (= 429 (io.github.cl-sdk.wst.routing:response-status second)))
      (5am:is-true (getf (io.github.cl-sdk.wst.routing:response-headers second) :retry-after)))))
