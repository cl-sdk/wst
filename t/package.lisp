(defpackage #:wst.routing.test
  (:use #:cl))

(in-package :wst.routing.test)

;;;
;;; wst.routing suite
;;;

(5am:def-suite wst.routing.suite
  :description "Tests for the wst.routing package.")

(5am:in-suite wst.routing.suite)

(defmacro def-route-testing (name args &body body)
  "Define a FiveAM test that resets all global routing state after running."
  (declare (ignorable args))
  `(5am:def-test ,name ()
     ,@body
     (setf wst.routing::*routes* nil
           wst.routing::*condition-handler* nil
           wst.routing::*any-route-handler* nil)))

;;; route macro, add-route, dispatch-route

(def-route-testing route-should-respond-when-dispatched ()
  (wst.routing:route test-route :GET "/testing-route" (request response)
    (declare (ignorable request))
    (wst.routing:ok-response t response :content "ok"))

  (5am:is-true (fboundp 'test-route))
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/testing-route"
                                                                  :method :GET))))
    (5am:is (equal "ok" (wst.routing:response-content rs)))))

(def-route-testing route-should-respond-with-404-when-dispatched-with-wrong-method ()
  (let ((response (wst.routing:dispatch-route (wst.routing:make-request :uri "/"
                                                                        :method :POST))))
    (5am:is (equal "not found" (wst.routing:response-content response)))))

(def-route-testing route-should-respond-with-default-not-found ()
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/x"
                                                                  :method :GET))))
    (5am:is (= 404 (wst.routing:response-status rs)))
    (5am:is (equal "not found" (wst.routing:response-content rs)))))

(def-route-testing return-internal-server-error-if-exception-is-thrown ()
  (wst.routing:add-route 'throw-exception "/throw-exception" :GET
                         (lambda (request response)
                           (declare (ignorable request response))
                           (error "something bad happened.")))
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/throw-exception"
                                                                  :method :GET))))
    (5am:is (equal "internal server error" (wst.routing:response-content rs)))))

;;; remove-route

(def-route-testing removing-a-route-makes-it-unreachable ()
  (wst.routing:add-route 'to-be-removed "/to-be-removed" :GET
                         (lambda (a b) (declare (ignorable a b)) t))
  (wst.routing:remove-route 'to-be-removed)
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/to-be-removed"
                                                                  :method :GET))))
    (5am:is (equal "not found" (wst.routing:response-content rs)))))

;;; find-route-by-name

(def-route-testing find-route-by-name-returns-registered-route ()
  (wst.routing:add-route 'my-route "/" :GET (lambda (req res) (declare (ignore req)) res))
  (let ((found (wst.routing:find-route-by-name 'my-route)))
    (5am:is-true found)
    (5am:is (eq 'my-route (wst.routing::route-name found)))))

;;; path parameters, with-request-data

(def-route-testing allow-parameters-on-path ()
  (wst.routing:add-route 'route-with-id "/r/:id" :GET
                         (lambda (request r)
                           (declare (ignore r))
                           (wst.routing:with-request-data (params)
                               request
                             (5am:is (equalp '(("id" . "6")) params)))))
  (wst.routing:dispatch-route (wst.routing:make-request :uri "/r/6" :method :GET)))

;;; with-request-params

(5am:def-test with-request-params-extracts-values-from-alist ()
  (let ((params '(("id" . "10") ("name" . "alice"))))
    (wst.routing:with-request-params (id name) params
      (5am:is (string-equal id "10"))
      (5am:is (string-equal name "alice")))))

(5am:def-test with-request-params-applies-transform-function ()
  (let ((params '(("count" . "5"))))
    (wst.routing:with-request-params ((count . #'parse-integer)) params
      (5am:is (= count 5)))))

;;; with-response-data

(5am:def-test with-response-data-extracts-values ()
  (let ((rs (wst.routing:make-response)))
    (setf (wst.routing:response-data rs) '(:token "abc"))
    (wst.routing:with-response-data (token) rs
      (5am:is (string-equal token "abc")))))

;;; parse-uri

(5am:def-test parse-request-uri-just-path ()
  (multiple-value-bind (uri query-string hash)
      (wst.routing:parse-uri "/a/b/c")
    (5am:is (string-equal uri "/a/b/c"))
    (5am:is (string-equal query-string ""))
    (5am:is (string-equal hash ""))))

(5am:def-test parse-request-uri-with-just-query ()
  (multiple-value-bind (uri query-string hash)
      (wst.routing:parse-uri "/a/b/c?ok=1")
    (5am:is (string-equal uri "/a/b/c"))
    (5am:is (string-equal query-string "ok=1"))
    (5am:is (string-equal hash ""))))

(5am:def-test parse-request-uri-with-just-hash ()
  (multiple-value-bind (uri query-string hash)
      (wst.routing:parse-uri "/a/b/c#anchor")
    (5am:is (string-equal uri "/a/b/c"))
    (5am:is (string-equal query-string ""))
    (5am:is (string-equal hash "anchor"))))

(5am:def-test parse-request-uri-with-query-and-hash ()
  (multiple-value-bind (uri query-string hash)
      (wst.routing:parse-uri "/a/b/c?ok=1#anchor")
    (5am:is (string-equal uri "/a/b/c"))
    (5am:is (string-equal query-string "ok=1"))
    (5am:is (string-equal hash "anchor"))))

(5am:def-test parse-request-root-uri-with-query-and-hash ()
  (multiple-value-bind (uri query-string hash)
      (wst.routing:parse-uri "/?ok=1#anchor")
    (5am:is (string-equal uri "/"))
    (5am:is (string-equal query-string "ok=1"))
    (5am:is (string-equal hash "anchor"))))

;;; route-uri-of

(def-route-testing route-uri-of-substitutes-path-params ()
  (wst.routing:add-route 'uri-gen-route "/users/:id/posts/:post-id" :GET
                         (lambda (req res) (declare (ignore req)) res))
  (let ((route (wst.routing:find-route-by-name 'uri-gen-route)))
    (5am:is (string-equal "/users/42/posts/7"
                          (wst.routing:route-uri-of route (list 42 7))))
    (5am:is (string-equal "/users/42/posts/7?page=1"
                          (wst.routing:route-uri-of route (list 42 7) :query "page=1")))))

;;; dispatch-route-by-route

(def-route-testing dispatch-route-by-route-calls-handler ()
  (wst.routing:add-route 'direct-route "/" :GET
                         (lambda (req res)
                           (declare (ignore req))
                           (setf (wst.routing:response-content res) "direct")
                           res))
  (let* ((route (wst.routing:find-route-by-name 'direct-route))
         (rs (wst.routing:dispatch-route-by-route
              route
              (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (string-equal "direct" (wst.routing:response-content rs)))))

;;; cookie parsing

(def-route-testing parse-request-cookies ()
  (wst.routing:add-route 'cookies "/" :GET
                         (lambda (request response)
                           (declare (ignorable response))
                           (let ((cookies (getf (wst.routing:request-data request) :cookies)))
                             (5am:is (= 2 (length (cl-hash-util:hash-keys cookies))))
                             response)))
  (wst.routing:dispatch-route (wst.routing:make-request
                               :uri "/"
                               :method :GET
                               :headers (cl-hash-util:hash ("cookie" "first=a; second=b")))))

;;; condition-handler

(def-route-testing customize-condition-handler ()
  (wst.routing:condition-handler
   (lambda (request response err)
     (declare (ignorable request))
     (5am:is (string-equal (simple-condition-format-control err) "something went wrong."))
     (setf (wst.routing:response-data response) '("meh"))
     response))
  (wst.routing:add-route 'customize-condition-handler "/" :GET
                         (lambda (request response)
                           (declare (ignorable request response))
                           (error "something went wrong.")))
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (string-equal (car (wst.routing:response-data rs)) "meh"))
    (wst.routing:remove-route 'customize-condition-handler)
    (wst.routing:condition-handler nil)))

(def-route-testing unhandled-customized-condition-handler-falls-back-to-500 ()
  (wst.routing:condition-handler
   (lambda (request response err)
     (declare (ignorable request response err))))
  (wst.routing:add-route 'unhandled-condition-route "/" :GET
                         (lambda (request response)
                           (declare (ignorable request response))
                           (error "something went wrong.")))
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 500 (wst.routing:response-status rs)))
    (wst.routing:remove-route 'unhandled-condition-route)
    (wst.routing:condition-handler nil)))

;;; any-route-handler

(def-route-testing any-route-with-method-matches-all-uris ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (setf (wst.routing:response-content response) "ok")
                                   response))
  (let ((a (wst.routing:dispatch-route (wst.routing:make-request :uri "/a" :method :GET)))
        (b (wst.routing:dispatch-route (wst.routing:make-request :uri "/b" :method :GET))))
    (5am:is (equal (wst.routing:response-content a)
                   (wst.routing:response-content b)))))

(def-route-testing dispatch-by-name-falls-through-to-any-route ()
  (let ((count 0))
    (wst.routing:any-route-handler :GET
                                   (lambda (request response)
                                     (declare (ignorable request))
                                     (setf count (1+ count))
                                     response))
    (wst.routing:dispatch-route-by-name 'a (wst.routing:make-request :uri "/a" :method :GET))
    (wst.routing:dispatch-route-by-name 'b (wst.routing:make-request :uri "/b" :method :GET))
    (5am:is (= count 2))))

(def-route-testing any-route-is-not-dispatched-when-method-differs ()
  (let ((count 0))
    (wst.routing:any-route-handler :GET
                                   (lambda (request response)
                                     (declare (ignorable request))
                                     (setf count (1+ count))
                                     response))
    (wst.routing:dispatch-route-by-name 'a (wst.routing:make-request :uri "/a" :method :POST))
    (5am:is (= count 0))))

;;; response helpers

(def-route-testing respond-with-ok ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:ok-response t response :content "done")
                                   response))
  (let ((rs (wst.routing:dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 200 (wst.routing:response-status rs)))
    (5am:is (string-equal "done" (wst.routing:response-content rs)))))

(def-route-testing respond-with-created ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:created-response t response)
                                   response))
  (5am:is (= 201 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-internal-server-error ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:internal-server-error-response t response)
                                   response))
  (5am:is (= 500 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-not-found ()
  (5am:is (= 404 (wst.routing:response-status
                  (wst.routing:dispatch-route
                   (wst.routing:make-request :uri "/nowhere" :method :GET))))))

(def-route-testing respond-with-unauthorized ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:unauthorized-response t response)
                                   response))
  (5am:is (= 401 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-forbidden ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:forbidden-response t response)
                                   response))
  (5am:is (= 403 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-bad-request ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:bad-request-response t response)
                                   response))
  (5am:is (= 400 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-too-many-requests ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:too-many-requests-response t response)
                                   response))
  (5am:is (= 429 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-unprocessable-entity ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:unprocessable-entity t response)
                                   response))
  (5am:is (= 422 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-not-implemented ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:not-implemented t response)
                                   response))
  (5am:is (= 501 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-redirect-see-other ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:redirect-see-other-response t response "/redirect")
                                   response))
  (let ((rs (wst.routing:dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 303 (wst.routing:response-status rs)))
    (5am:is (string-equal (getf (wst.routing:response-headers rs) :location) "/redirect"))))

(defmethod wst.routing:ok-response ((ty (eql :sexp)) response &key headers content)
  (declare (ignorable headers))
  (wst.routing:write-response response :status 200
                                       :content-type "application/s-expression"
                                       :headers headers
                                       :content (format nil "~a" content)))

(def-route-testing respond-with-custom-ok-response-method ()
  (wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (wst.routing:ok-response :sexp response :content (list 1 2 3))
                                   response))
  (let ((rs (wst.routing:dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 200 (wst.routing:response-status rs)))
    (5am:is (string-equal (getf (wst.routing:response-headers rs) :content-type)
                          "application/s-expression"))
    (5am:is (equal (wst.routing:response-content rs) "(1 2 3)"))))

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
  (wst.routing.dsl:build-webserver
   `(wst.routing.dsl:route :GET index "/" route-responder))
  (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET)))

(def-route-testing build-with-just-route-is-the-same-as-plain-route ()
  (let* ((count 0)
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :route (wst.routing.dsl:route :GET index "/" ,handler)))
    (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET))
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
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,before-action
       :route (wst.routing.dsl:route :GET index "/" ,handler)))
    (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-route-with-just-after-middleware ()
  (let* ((count 0)
         (after-action (lambda (req res)
                         (declare (ignore req res))
                         (setf count (1+ count))))
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :route (wst.routing.dsl:route :GET index "/" ,handler)
       :after ,after-action))
    (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-route-with-before-and-after-middleware ()
  (let* ((count 0)
         (middleware (lambda (req res)
                       (declare (ignore req res))
                       (setf count (1+ count))
                       (cons :continue res))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,middleware
       :route (wst.routing.dsl:route :GET index "/" ,middleware)
       :after ,middleware))
    (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET))
    (5am:is (= 3 count))))

(def-route-testing build-group-of-routes ()
  (let* ((count 0)
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:group
       (wst.routing.dsl:route :GET route-a "/a" ,handler)
       (wst.routing.dsl:route :GET route-b "/b" ,handler)))
    (wst.routing:dispatch-route-by-name 'route-a (wst.routing:make-request :method :GET))
    (wst.routing:dispatch-route-by-name 'route-b (wst.routing:make-request :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-resource-routes-with-prefix ()
  (let* ((count 0)
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:resource "/base"
                                (wst.routing.dsl:route :GET route-a "/a" ,handler)
                                (wst.routing.dsl:route :GET route-b ,handler)))
    (wst.routing:dispatch-route (wst.routing:make-request :uri "/base/a" :method :GET))
    (wst.routing:dispatch-route (wst.routing:make-request :uri "/base" :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-any-route-catches-all-uris ()
  (let* ((count 0)
         (handler (lambda (req res)
                    (declare (ignore req res))
                    (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:any-route :GET ,handler))
    (wst.routing:dispatch-route (wst.routing:make-request :uri "/anything" :method :GET))
    (5am:is (= 1 count))))

(def-route-testing build-route-with-custom-metadata ()
  (let* ((handler (lambda (req res) (declare (ignore req)) res)))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:group
       (wst.routing.dsl:route :GET route-a "/" ,handler :custom :ok)
       (wst.routing.dsl:resource "/a"
                                 (wst.routing.dsl:route :GET route-b ,handler :custom :ok))))
    (5am:is-true (equal :ok (car (wst.routing::route-custom
                                  (wst.routing:find-route-by-name 'route-a)))))
    (5am:is-true (equal :ok (car (wst.routing::route-custom
                                  (wst.routing:find-route-by-name 'route-b)))))))

(def-route-testing rate-limit-throttles-after-limit-is-reached ()
  (let ((handler (lambda (req res)
                   (declare (ignore req))
                   (wst.routing:ok-response t res :content "ok")
                   res)))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,(wst.throttle:rate-limit :max-requests 1 :window-seconds 60)
       :route (wst.routing.dsl:route :GET throttled "/" ,handler)))
    (let ((first (wst.routing:dispatch-route (wst.routing:make-request :uri "/" :method :GET)))
          (second (wst.routing:dispatch-route (wst.routing:make-request :uri "/" :method :GET))))
      (5am:is (= 200 (wst.routing:response-status first)))
      (5am:is (= 429 (wst.routing:response-status second)))
      (5am:is-true (getf (wst.routing:response-headers second) :retry-after)))))

;;;
;;; wst.routing.woo suite
;;;

(5am:def-suite wst.routing.woo.suite
  :description "Tests for the wst.routing.woo adapter package.")

(5am:in-suite wst.routing.woo.suite)

(5am:def-test request-from-woo-env-parses-uri-components ()
  (let* ((env (list :request-uri "/users/1?foo=bar#section"
                    :headers (cl-hash-util:hash ("content-type" "text/html"))
                    :request-method :GET
                    :content-type "text/html"
                    :content-length 42
                    :raw-body nil))
         (req (wst.routing.woo:request-from-woo-env env)))
    (5am:is (string-equal "/users/1" (wst.routing:request-uri req)))
    (5am:is (string-equal "foo=bar" (wst.routing:request-query req)))
    (5am:is (eql :GET (wst.routing:request-method req)))
    (5am:is (= 42 (wst.routing:request-content-length req)))))

(5am:def-test request-from-woo-env-stores-original-env ()
  (let* ((env (list :request-uri "/"
                    :headers (cl-hash-util:hash)
                    :request-method :POST
                    :content-type nil
                    :content-length 0
                    :raw-body nil))
         (req (wst.routing.woo:request-from-woo-env env)))
    (5am:is-true (getf (wst.routing:request-data req) :env))
    (5am:is (eq env (getf (wst.routing:request-data req) :env)))))

(5am:def-test response-to-woo-response-returns-correct-format ()
  (let* ((rs (wst.routing:make-response)))
    (setf (wst.routing:response-status rs) 200
          (wst.routing:response-headers rs) (list :content-type "text/plain")
          (wst.routing:response-content rs) "hello")
    (let ((woo-rs (wst.routing.woo:response-to-woo-response rs)))
      (5am:is (= 200 (first woo-rs)))
      (5am:is (equal (list :content-type "text/plain") (second woo-rs)))
      (5am:is (equal (list "hello") (third woo-rs))))))

;;;
;;; wst.routing.response.dsl suite
;;;

(defpackage #:wst.routing.response.dsl.test
  (:use #:cl))

(in-package :wst.routing.response.dsl.test)

(5am:def-suite wst.routing.response.dsl.suite
  :description "Tests for the wst.routing.response.dsl package.")

(5am:in-suite wst.routing.response.dsl.suite)

;;; headers

(5am:def-test set-no-headers-leaves-response-headers-nil ()
  (let ((target (wst.routing:make-response)))
    (wst.routing.response.dsl:headers nil target)
    (5am:is-true (null (wst.routing:response-headers target)))))

(5am:def-test set-a-single-header ()
  (let ((target (wst.routing:make-response)))
    (wst.routing.response.dsl:headers (list :content-type "mimetype") target)
    (5am:is-true (string-equal (getf (wst.routing:response-headers target) :content-type)
                               "mimetype"))))

(5am:def-test setting-headers-replaces-existing-values ()
  (let ((target (wst.routing:make-response)))
    (wst.routing.response.dsl:headers (list :content-type "mimetype" :content-length 0) target)
    (wst.routing.response.dsl:headers (list :content-type "mimetype2") target)
    (5am:is-true (string-equal (getf (wst.routing:response-headers target) :content-type)
                               "mimetype2"))))

(5am:def-test headers-returns-the-response-for-chaining ()
  (let* ((target (wst.routing:make-response))
         (returned (wst.routing.response.dsl:headers (list :x-custom "yes") target)))
    (5am:is (eq target returned))))

;;; status

(5am:def-test set-status-updates-response-status-code ()
  (let ((target (wst.routing:make-response)))
    (wst.routing.response.dsl:status wst.http:+http-status-200+ target)
    (5am:is (= wst.http:+http-status-200+ (wst.routing:response-status target)))))

(5am:def-test setting-invalid-status-signals-type-error ()
  (5am:signals type-error
    (wst.routing.response.dsl:status nil (wst.routing:make-response))))

;;; text / html / json body helpers

(5am:def-test set-text-body-sets-content-type-and-content ()
  (let ((target (wst.routing.response.dsl:text "hello" (wst.routing:make-response))))
    (5am:is (string-equal "text/plain"
                          (getf (wst.routing:response-headers target) :content-type)))
    (5am:is (string-equal "hello" (wst.routing:response-content target)))))

(5am:def-test set-html-body-sets-content-type-and-content ()
  (let ((target (wst.routing.response.dsl:html t "<p>hi</p>" (wst.routing:make-response))))
    (5am:is (string-equal "text/html"
                          (getf (wst.routing:response-headers target) :content-type)))
    (5am:is (string-equal "<p>hi</p>" (wst.routing:response-content target)))))

(5am:def-test set-json-body-sets-content-type-and-content ()
  (let ((target (wst.routing.response.dsl:json t "{}" (wst.routing:make-response))))
    (5am:is (string-equal "application/json"
                          (getf (wst.routing:response-headers target) :content-type)))
    (5am:is (string-equal "{}" (wst.routing:response-content target)))))
