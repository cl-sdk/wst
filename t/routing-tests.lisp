(in-package :io.github.cl-sdk.wst.test)

;;;
;;; wst.routing suite
;;;

(5am:def-suite wst.routing.suite
  :description "Tests for the wst.routing package.")

(5am:in-suite wst.routing.suite)

;;; route macro, add-route, dispatch-route

(def-route-testing route-should-respond-when-dispatched ()
  (io.github.cl-sdk.wst.routing:route test-route :GET "/testing-route" (request response)
    (declare (ignorable request))
    (io.github.cl-sdk.wst.routing:ok-response t response :content "ok"))

  (5am:is-true (fboundp 'test-route))
  (let ((rs (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/testing-route"
                                                                  :method :GET))))
    (5am:is (equal "ok" (io.github.cl-sdk.wst.routing:response-content rs)))))

(def-route-testing route-should-respond-with-404-when-dispatched-with-wrong-method ()
  (let ((response (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/"
                                                                        :method :POST))))
    (5am:is (equal "not found" (io.github.cl-sdk.wst.routing:response-content response)))))

(def-route-testing route-should-respond-with-default-not-found ()
  (let ((rs (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/x"
                                                                  :method :GET))))
    (5am:is (= 404 (io.github.cl-sdk.wst.routing:response-status rs)))
    (5am:is (equal "not found" (io.github.cl-sdk.wst.routing:response-content rs)))))

(def-route-testing return-internal-server-error-if-exception-is-thrown ()
  (io.github.cl-sdk.wst.routing:add-route 'throw-exception "/throw-exception" :GET
                         (lambda (request response)
                           (declare (ignorable request response))
                           (error "something bad happened.")))
  (let ((rs (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/throw-exception"
                                                                  :method :GET))))
    (5am:is (equal "internal server error" (io.github.cl-sdk.wst.routing:response-content rs)))))

;;; remove-route

(def-route-testing removing-a-route-makes-it-unreachable ()
  (io.github.cl-sdk.wst.routing:add-route 'to-be-removed "/to-be-removed" :GET
                         (lambda (a b) (declare (ignorable a b)) t))
  (io.github.cl-sdk.wst.routing:remove-route 'to-be-removed)
  (let ((rs (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/to-be-removed"
                                                                  :method :GET))))
    (5am:is (equal "not found" (io.github.cl-sdk.wst.routing:response-content rs)))))

;;; find-route-by-name

(def-route-testing find-route-by-name-returns-registered-route ()
  (io.github.cl-sdk.wst.routing:add-route 'my-route "/" :GET (lambda (req res) (declare (ignore req)) res))
  (let ((found (io.github.cl-sdk.wst.routing:find-route-by-name 'my-route)))
    (5am:is-true found)
    (5am:is (eq 'my-route (io.github.cl-sdk.wst.routing::route-name found)))))

;;; path parameters, with-request-data

(def-route-testing allow-parameters-on-path ()
  (io.github.cl-sdk.wst.routing:add-route 'route-with-id "/r/:id" :GET
                         (lambda (request r)
                           (declare (ignore r))
                           (io.github.cl-sdk.wst.routing:with-request-data (params)
                               request
                             (5am:is (equalp '(("id" . "6")) params)))))
  (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/r/6" :method :GET)))

;;; with-request-params

(5am:def-test with-request-params-extracts-values-from-alist ()
  (let ((params '(("id" . "10") ("name" . "alice"))))
    (io.github.cl-sdk.wst.routing:with-request-params (id name) params
      (5am:is (string-equal id "10"))
      (5am:is (string-equal name "alice")))))

(5am:def-test with-request-params-applies-transform-function ()
  (let ((params '(("count" . "5"))))
    (io.github.cl-sdk.wst.routing:with-request-params ((count . #'parse-integer)) params
      (5am:is (= count 5)))))

;;; request-header

(5am:def-test request-header-matches ()
  (let ((request (io.github.cl-sdk.wst.routing:make-request
                  :headers (cl-hash-util:hash ("Content-Type" "text/plain")))))
    (5am:is (string= "text/plain"
                     (io.github.cl-sdk.wst.routing:request-header request "Content-Type")))))

(5am:def-test request-header-returns-nil-when-missing ()
  (let ((request (io.github.cl-sdk.wst.routing:make-request
                  :headers (cl-hash-util:hash ("Content-Type" "text/plain")))))
    (5am:is (null (io.github.cl-sdk.wst.routing:request-header request "Authorization")))))

(5am:def-test request-header-returns-default-value-when-missing ()
  (let ((request (io.github.cl-sdk.wst.routing:make-request
                  :headers (cl-hash-util:hash ("Content-Type" "text/plain")))))
    (5am:is (string-equal
             "default"
             (io.github.cl-sdk.wst.routing:request-header request "Authorization" "default")))))

;;; with-response-data

(5am:def-test with-response-data-extracts-values ()
  (let ((rs (io.github.cl-sdk.wst.routing:make-response)))
    (setf (io.github.cl-sdk.wst.routing:response-data rs) '(:token "abc"))
    (io.github.cl-sdk.wst.routing:with-response-data (token) rs
      (5am:is (string-equal token "abc")))))

;;; append-request-data

(5am:def-test append-request-data-adds-key-value-pair ()
  (let ((request (io.github.cl-sdk.wst.routing:make-request)))
    (io.github.cl-sdk.wst.routing:append-request-data request :session "my-session")
    (5am:is (string-equal "my-session"
                          (getf (io.github.cl-sdk.wst.routing:request-data request) :session)))))

(5am:def-test append-request-data-returns-request ()
  (let ((request (io.github.cl-sdk.wst.routing:make-request)))
    (5am:is (eq request
                (io.github.cl-sdk.wst.routing:append-request-data request :key "value")))))

(5am:def-test append-request-data-accumulates-multiple-pairs ()
  (let ((request (io.github.cl-sdk.wst.routing:make-request)))
    (io.github.cl-sdk.wst.routing:append-request-data request :a 1)
    (io.github.cl-sdk.wst.routing:append-request-data request :b 2)
    (5am:is (= 1 (getf (io.github.cl-sdk.wst.routing:request-data request) :a)))
    (5am:is (= 2 (getf (io.github.cl-sdk.wst.routing:request-data request) :b)))))

;;; remove-request-data

(5am:def-test remove-request-data-removes-key-value-pair ()
  (let ((request (io.github.cl-sdk.wst.routing:make-request :data '(:session "my-session"))))
    (io.github.cl-sdk.wst.routing:remove-request-data request :session)
    (5am:is (null (getf (io.github.cl-sdk.wst.routing:request-data request) :session)))))

(5am:def-test remove-request-data-returns-request ()
  (let ((request (io.github.cl-sdk.wst.routing:make-request :data '(:session "my-session"))))
    (5am:is (eq request
                (io.github.cl-sdk.wst.routing:remove-request-data request :session)))))

(5am:def-test remove-request-data-leaves-other-pairs-intact ()
  (let ((request (io.github.cl-sdk.wst.routing:make-request :data '(:a 1 :b 2))))
    (io.github.cl-sdk.wst.routing:remove-request-data request :a)
    (5am:is (null (getf (io.github.cl-sdk.wst.routing:request-data request) :a)))
    (5am:is (= 2 (getf (io.github.cl-sdk.wst.routing:request-data request) :b)))))

;;; wst.request-content

(5am:def-test parse-content-type-parses-simple-type ()
  (let ((result (io.github.cl-sdk.wst.request-content:parse-content-type "text/plain")))
    (5am:is (= 1 (length result)))
    (5am:is (eql :|text/plain| (car (first result))))
    (5am:is (null (cdr (first result))))))

(5am:def-test parse-content-type-parses-type-with-options ()
  (let ((result (io.github.cl-sdk.wst.request-content:parse-content-type
                 "application/x-www-form-urlencoded; charset=utf-8")))
    (5am:is (= 1 (length result)))
    (5am:is (eql :|application/x-www-form-urlencoded| (car (first result))))
    (5am:is (equal '(("charset" . "utf-8")) (cdr (first result))))))

(5am:def-test parse-content-type-parses-multiple-types ()
  (let ((result (io.github.cl-sdk.wst.request-content:parse-content-type
                 "text/plain, application/x-www-form-urlencoded; q=0.9")))
    (5am:is (= 2 (length result)))
    (5am:is (eql :|text/plain| (car (first result))))
    (5am:is (null (cdr (first result))))
    (5am:is (eql :|application/x-www-form-urlencoded| (car (second result))))
    (5am:is (equal '(("q" . "0.9")) (cdr (second result))))))

(5am:def-test parse-content-type-returns-nil-for-empty-header ()
  (5am:is (null (io.github.cl-sdk.wst.request-content:parse-content-type "")))
  (5am:is (null (io.github.cl-sdk.wst.request-content:parse-content-type "   "))))

(5am:def-test parse-content-type-signals-error-for-non-string ()
  (5am:signals type-error (io.github.cl-sdk.wst.request-content:parse-content-type nil))
  (5am:signals type-error (io.github.cl-sdk.wst.request-content:parse-content-type 42)))

(5am:def-test parse-content-type-unquotes-quoted-string-parameter ()
  ;; RFC 7230 §3.2.6: parameter values may be quoted-strings
  (let ((result (io.github.cl-sdk.wst.request-content:parse-content-type
                 "text/plain; charset=\"utf-8\"")))
    (5am:is (= 1 (length result)))
    (5am:is (eql :|text/plain| (car (first result))))
    (5am:is (equal '(("charset" . "utf-8")) (cdr (first result))))))

(5am:def-test parse-content-type-lowercases-parameter-names ()
  ;; RFC 7231 §3.1.1.1: parameter names are case-insensitive
  (let ((result (io.github.cl-sdk.wst.request-content:parse-content-type
                 "text/plain; Charset=utf-8")))
    (5am:is (= 1 (length result)))
    (5am:is (equal '(("charset" . "utf-8")) (cdr (first result))))))

(5am:def-test parse-content-parses-form-urlencoded ()
  (5am:is (equal '(("name" . "alice smith") ("email" . "alice@test.dev"))
                 (io.github.cl-sdk.wst.request-content:parse-content
                  :|application/x-www-form-urlencoded|
                  "name=alice+smith&email=alice%40test.dev"))))

(5am:def-test parse-content-text-plain-returns-content-as-string ()
  (5am:is (string= "hello"
                   (io.github.cl-sdk.wst.request-content:parse-content :|text/plain| "hello"))))

(5am:def-test parse-content-default-returns-content-as-string ()
  (5am:is (string= "hello"
                   (io.github.cl-sdk.wst.request-content:parse-content :unknown-type "hello"))))

(5am:def-test content-as-string-reads-from-character-stream ()
  (5am:is (string= "hello stream"
                   (io.github.cl-sdk.wst.request-content:content-as-string
                    (make-string-input-stream "hello stream")))))

(5am:def-test content-as-string-reads-from-binary-stream ()
  (let ((stream (flexi-streams:make-in-memory-input-stream #(104 101 108 108 111))))
    (5am:is (string= "hello"
                     (io.github.cl-sdk.wst.request-content:content-as-string stream)))))

;;; parse-uri

(5am:def-test parse-request-uri-just-path ()
  (multiple-value-bind (uri query-string hash)
      (io.github.cl-sdk.wst.routing:parse-uri "/a/b/c")
    (5am:is (string-equal uri "/a/b/c"))
    (5am:is (string-equal query-string ""))
    (5am:is (string-equal hash ""))))

(5am:def-test parse-request-uri-with-just-query ()
  (multiple-value-bind (uri query-string hash)
      (io.github.cl-sdk.wst.routing:parse-uri "/a/b/c?ok=1")
    (5am:is (string-equal uri "/a/b/c"))
    (5am:is (string-equal query-string "ok=1"))
    (5am:is (string-equal hash ""))))

(5am:def-test parse-request-uri-with-just-hash ()
  (multiple-value-bind (uri query-string hash)
      (io.github.cl-sdk.wst.routing:parse-uri "/a/b/c#anchor")
    (5am:is (string-equal uri "/a/b/c"))
    (5am:is (string-equal query-string ""))
    (5am:is (string-equal hash "anchor"))))

(5am:def-test parse-request-uri-with-query-and-hash ()
  (multiple-value-bind (uri query-string hash)
      (io.github.cl-sdk.wst.routing:parse-uri "/a/b/c?ok=1#anchor")
    (5am:is (string-equal uri "/a/b/c"))
    (5am:is (string-equal query-string "ok=1"))
    (5am:is (string-equal hash "anchor"))))

(5am:def-test parse-request-root-uri-with-query-and-hash ()
  (multiple-value-bind (uri query-string hash)
      (io.github.cl-sdk.wst.routing:parse-uri "/?ok=1#anchor")
    (5am:is (string-equal uri "/"))
    (5am:is (string-equal query-string "ok=1"))
    (5am:is (string-equal hash "anchor"))))

;;; route-uri-of

(def-route-testing route-uri-of-substitutes-path-params ()
  (io.github.cl-sdk.wst.routing:add-route 'uri-gen-route "/users/:id/posts/:post-id" :GET
                         (lambda (req res) (declare (ignore req)) res))
  (let ((route (io.github.cl-sdk.wst.routing:find-route-by-name 'uri-gen-route)))
    (5am:is (string-equal "/users/42/posts/7"
                          (io.github.cl-sdk.wst.routing:route-uri-of route (list 42 7))))
    (5am:is (string-equal "/users/42/posts/7?page=1"
                          (io.github.cl-sdk.wst.routing:route-uri-of route (list 42 7) :query "page=1")))))

;;; dispatch-route-by-route

(def-route-testing dispatch-route-by-route-calls-handler ()
  (io.github.cl-sdk.wst.routing:add-route 'direct-route "/" :GET
                         (lambda (req res)
                           (declare (ignore req))
                           (setf (io.github.cl-sdk.wst.routing:response-content res) "direct")
                           res))
  (let* ((route (io.github.cl-sdk.wst.routing:find-route-by-name 'direct-route))
         (rs (io.github.cl-sdk.wst.routing:dispatch-route-by-route
              route
              (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (string-equal "direct" (io.github.cl-sdk.wst.routing:response-content rs)))))

;;; cookie parsing

(def-route-testing parse-request-cookies ()
  (io.github.cl-sdk.wst.routing:add-route 'cookies "/" :GET
                                          (lambda (request response)
                                            (let ((cookies (io.github.cl-sdk.wst.cookies:parse-cookies
                                                            (io.github.cl-sdk.wst.routing:request-headers request))))
                                              (5am:is (= 2 (length cookies)))
                                              (5am:is (string-equal "a"
                                                                    (io.github.cl-sdk.wst.cookies:cookie-value
                                                                     (find "first" cookies
                                                                           :key #'io.github.cl-sdk.wst.cookies:cookie-name
                                                                           :test #'string-equal)))))
                                            response))
  (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request
                               :uri "/"
                               :method :GET
                               :headers (cl-hash-util:hash ("cookie" "first=a; second=b")))))

;;; condition-handler

(def-route-testing customize-condition-handler ()
  (io.github.cl-sdk.wst.routing:condition-handler
   (lambda (request response err)
     (declare (ignorable request))
     (5am:is (string-equal (simple-condition-format-control err) "something went wrong."))
     (setf (io.github.cl-sdk.wst.routing:response-data response) '("meh"))
     response))
  (io.github.cl-sdk.wst.routing:add-route 'customize-condition-handler "/" :GET
                         (lambda (request response)
                           (declare (ignorable request response))
                           (error "something went wrong.")))
  (let ((rs (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (string-equal (car (io.github.cl-sdk.wst.routing:response-data rs)) "meh"))
    (io.github.cl-sdk.wst.routing:remove-route 'customize-condition-handler)
    (io.github.cl-sdk.wst.routing:condition-handler nil)))

(def-route-testing unhandled-customized-condition-handler-falls-back-to-500 ()
  (io.github.cl-sdk.wst.routing:condition-handler
   (lambda (request response err)
     (declare (ignorable request response err))))
  (io.github.cl-sdk.wst.routing:add-route 'unhandled-condition-route "/" :GET
                         (lambda (request response)
                           (declare (ignorable request response))
                           (error "something went wrong.")))
  (let ((rs (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 500 (io.github.cl-sdk.wst.routing:response-status rs)))
    (io.github.cl-sdk.wst.routing:remove-route 'unhandled-condition-route)
    (io.github.cl-sdk.wst.routing:condition-handler nil)))

;;; any-route-handler

(def-route-testing any-route-with-method-matches-all-uris ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (setf (io.github.cl-sdk.wst.routing:response-content response) "ok")
                                   response))
  (let ((a (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/a" :method :GET)))
        (b (io.github.cl-sdk.wst.routing:dispatch-route (io.github.cl-sdk.wst.routing:make-request :uri "/b" :method :GET))))
    (5am:is (equal (io.github.cl-sdk.wst.routing:response-content a)
                   (io.github.cl-sdk.wst.routing:response-content b)))))

(def-route-testing dispatch-by-name-falls-through-to-any-route ()
  (let ((count 0))
    (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                   (lambda (request response)
                                     (declare (ignorable request))
                                     (setf count (1+ count))
                                     response))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'a (io.github.cl-sdk.wst.routing:make-request :uri "/a" :method :GET))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'b (io.github.cl-sdk.wst.routing:make-request :uri "/b" :method :GET))
    (5am:is (= count 2))))

(def-route-testing any-route-is-not-dispatched-when-method-differs ()
  (let ((count 0))
    (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                   (lambda (request response)
                                     (declare (ignorable request))
                                     (setf count (1+ count))
                                     response))
    (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'a (io.github.cl-sdk.wst.routing:make-request :uri "/a" :method :POST))
    (5am:is (= count 0))))

;;; response helpers

(def-route-testing respond-with-ok ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:ok-response t response :content "done")
                                   response))
  (let ((rs (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status rs)))
    (5am:is (string-equal "done" (io.github.cl-sdk.wst.routing:response-content rs)))))

(def-route-testing respond-with-created ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:created-response t response)
                                   response))
  (5am:is (= 201 (io.github.cl-sdk.wst.routing:response-status
                  (io.github.cl-sdk.wst.routing:dispatch-route-by-name
                   'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-internal-server-error ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:internal-server-error-response t response)
                                   response))
  (5am:is (= 500 (io.github.cl-sdk.wst.routing:response-status
                  (io.github.cl-sdk.wst.routing:dispatch-route-by-name
                   'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-not-found ()
  (5am:is (= 404 (io.github.cl-sdk.wst.routing:response-status
                  (io.github.cl-sdk.wst.routing:dispatch-route
                   (io.github.cl-sdk.wst.routing:make-request :uri "/nowhere" :method :GET))))))

(def-route-testing respond-with-unauthorized ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:unauthorized-response t response)
                                   response))
  (5am:is (= 401 (io.github.cl-sdk.wst.routing:response-status
                  (io.github.cl-sdk.wst.routing:dispatch-route-by-name
                   'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-forbidden ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:forbidden-response t response)
                                   response))
  (5am:is (= 403 (io.github.cl-sdk.wst.routing:response-status
                  (io.github.cl-sdk.wst.routing:dispatch-route-by-name
                   'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-bad-request ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:bad-request-response t response)
                                   response))
  (5am:is (= 400 (io.github.cl-sdk.wst.routing:response-status
                  (io.github.cl-sdk.wst.routing:dispatch-route-by-name
                   'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-too-many-requests ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:too-many-requests-response t response)
                                   response))
  (5am:is (= 429 (io.github.cl-sdk.wst.routing:response-status
                  (io.github.cl-sdk.wst.routing:dispatch-route-by-name
                   'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-unprocessable-entity ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:unprocessable-entity t response)
                                   response))
  (5am:is (= 422 (io.github.cl-sdk.wst.routing:response-status
                  (io.github.cl-sdk.wst.routing:dispatch-route-by-name
                   'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-not-implemented ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:not-implemented t response)
                                   response))
  (5am:is (= 501 (io.github.cl-sdk.wst.routing:response-status
                  (io.github.cl-sdk.wst.routing:dispatch-route-by-name
                   'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-redirect-see-other ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:redirect-see-other-response t response "/redirect")
                                   response))
  (let ((rs (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 303 (io.github.cl-sdk.wst.routing:response-status rs)))
    (5am:is (string-equal (getf (io.github.cl-sdk.wst.routing:response-headers rs) :location) "/redirect"))))

(defmethod io.github.cl-sdk.wst.routing:ok-response ((ty (eql :sexp)) response &key headers content)
  (declare (ignorable headers))
  (io.github.cl-sdk.wst.routing:write-response
   response
   200
   headers
   (format nil "~a" content)
   :content-type "application/s-expression"))

(def-route-testing respond-with-custom-ok-response-method ()
  (io.github.cl-sdk.wst.routing:any-route-handler :GET
                                 (lambda (request response)
                                   (declare (ignorable request))
                                   (io.github.cl-sdk.wst.routing:ok-response :sexp response :content (list 1 2 3))
                                   response))
  (let ((rs (io.github.cl-sdk.wst.routing:dispatch-route-by-name 'a (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 200 (io.github.cl-sdk.wst.routing:response-status rs)))
    (5am:is (string-equal (getf (io.github.cl-sdk.wst.routing:response-headers rs) :content-type)
                          "application/s-expression"))
    (5am:is (equal (io.github.cl-sdk.wst.routing:response-content rs) "(1 2 3)"))))
