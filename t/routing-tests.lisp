(in-package :wst.routing.test)

;;;
;;; wst.routing suite
;;;

(5am:def-suite wst.routing.suite
  :description "Tests for the wst.routing package.")

(5am:in-suite wst.routing.suite)

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

;;; wst.request-content

(5am:def-test parse-content-type-parses-simple-type ()
  (let ((result (wst.request-content:parse-content-type "text/plain")))
    (5am:is (= 1 (length result)))
    (5am:is (eql :|text/plain| (car (first result))))
    (5am:is (null (cdr (first result))))))

(5am:def-test parse-content-type-parses-type-with-options ()
  (let ((result (wst.request-content:parse-content-type
                 "application/x-www-form-urlencoded; charset=utf-8")))
    (5am:is (= 1 (length result)))
    (5am:is (eql :|application/x-www-form-urlencoded| (car (first result))))
    (5am:is (equal '(("charset" . "utf-8")) (cdr (first result))))))

(5am:def-test parse-content-type-parses-multiple-types ()
  (let ((result (wst.request-content:parse-content-type
                 "text/plain, application/x-www-form-urlencoded; q=0.9")))
    (5am:is (= 2 (length result)))
    (5am:is (eql :|text/plain| (car (first result))))
    (5am:is (null (cdr (first result))))
    (5am:is (eql :|application/x-www-form-urlencoded| (car (second result))))
    (5am:is (equal '(("q" . "0.9")) (cdr (second result))))))

(5am:def-test parse-content-type-returns-nil-for-empty-header ()
  (5am:is (null (wst.request-content:parse-content-type "")))
  (5am:is (null (wst.request-content:parse-content-type "   "))))

(5am:def-test parse-content-type-signals-error-for-non-string ()
  (5am:signals type-error (wst.request-content:parse-content-type nil))
  (5am:signals type-error (wst.request-content:parse-content-type 42)))

(5am:def-test parse-content-type-unquotes-quoted-string-parameter ()
  ;; RFC 7230 §3.2.6: parameter values may be quoted-strings
  (let ((result (wst.request-content:parse-content-type
                 "text/plain; charset=\"utf-8\"")))
    (5am:is (= 1 (length result)))
    (5am:is (eql :|text/plain| (car (first result))))
    (5am:is (equal '(("charset" . "utf-8")) (cdr (first result))))))

(5am:def-test parse-content-type-lowercases-parameter-names ()
  ;; RFC 7231 §3.1.1.1: parameter names are case-insensitive
  (let ((result (wst.request-content:parse-content-type
                 "text/plain; Charset=utf-8")))
    (5am:is (= 1 (length result)))
    (5am:is (equal '(("charset" . "utf-8")) (cdr (first result))))))

(5am:def-test parse-content-parses-form-urlencoded ()
  (5am:is (equal '(("name" . "alice smith") ("email" . "alice@test.dev"))
                 (wst.request-content:parse-content
                  :|application/x-www-form-urlencoded|
                  "name=alice+smith&email=alice%40test.dev"))))

(5am:def-test parse-content-text-plain-returns-content-as-string ()
  (5am:is (string= "hello"
                   (wst.request-content:parse-content :|text/plain| "hello"))))

(5am:def-test parse-content-default-returns-content-as-string ()
  (5am:is (string= "hello"
                   (wst.request-content:parse-content :unknown-type "hello"))))

(5am:def-test content-as-string-reads-from-character-stream ()
  (5am:is (string= "hello stream"
                   (wst.request-content:content-as-string
                    (make-string-input-stream "hello stream")))))

(5am:def-test content-as-string-reads-from-binary-stream ()
  (let ((stream (flexi-streams:make-in-memory-input-stream #(104 101 108 108 111))))
    (5am:is (string= "hello"
                     (wst.request-content:content-as-string stream)))))

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
                             (5am:is (= 2 (length cookies)))
                             (5am:is (string-equal "a"
                                                   (wst.cookies:cookie-value
                                                    (find "first" cookies
                                                          :key #'wst.cookies:cookie-name
                                                          :test #'string-equal))))
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

(def-route-testing built-in-development-condition-handler-adds-debug-details ()
  (wst.routing:condition-handler #'wst.routing:development-condition-handler)
  (wst.routing:add-route 'dev-condition-route "/oops" :GET
                         (lambda (request response)
                           (declare (ignorable request response))
                           (error "something went wrong.")))
  (let* ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/oops" :method :GET)))
         (content (wst.routing:response-content rs)))
    (5am:is (= 500 (wst.routing:response-status rs)))
    (5am:is (search "condition handled" content))
    (5am:is (search "method: GET" content))
    (5am:is (search "uri: /oops" content))
    (5am:is (search "message: something went wrong." content))
    (5am:is (search "stack trace:" content))
    (wst.routing:remove-route 'dev-condition-route)
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
