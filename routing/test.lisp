(defpackage #:wst.routing.test
  (:use #:cl)
  (:import-from #:cl-hash-util
                #:hash-create)
  (:import-from #:wst.routing
                #:response-status
                #:response-headers
                #:write-response
                #:ok-response
                #:dispatch-route-by-name
                #:response-data
                #:response-content
                #:make-request
                #:add-route
                #:remove-route
                #:dispatch-route
                #:response
                #:route))

(in-package :wst.routing.test)

(5am:def-suite* wst.routing.suite)

(route test-route :GET "/testing-route" (request response)
  (declare (ignorable request))
  (ok-response t response :content "ok"))

(5am:def-test route-was-compiled ()
  (5am:is-true (fboundp 'test-route)))

(5am:def-test route-should-respond-when-dispatched ()
  (let ((rs (dispatch-route (wst.routing:make-request :uri "/testing-route"
                                                      :method :GET))))
    (5am:is (equal "ok" (response-content rs)))))

(5am:def-test route-should-respond-with-404-when-dispatched-with-wrong-method ()
  (let ((response (dispatch-route (wst.routing:make-request :uri "/"
                                                            :method :POST))))
    (5am:is (equal "not found" (response-content response)))))

(5am:def-test removing-test-route ()
  (add-route 'to-be-remove "/to-be-removed" :GET (lambda (a b) (declare (ignorable a b)) t))
  (remove-route 'to-be-remove)
  (let ((rs (dispatch-route (wst.routing:make-request :uri "/to-be-removed"
                                                      :method :GET))))
    (5am:is (equal "not found" (response-content rs)))))

(5am:def-test allow-parameters-on-path ()
  (add-route 'route-with-id
             "/r/:id"
             :GET
             (lambda (request r)
               (declare (ignore r))
               (getf (wst.routing:request-data request) :params)))
  (let* ((request (make-request :uri "/r/6" :method :GET)))
    (dispatch-route request)
    (5am:is (equalp '(("id" . "6")) (getf (wst.routing:request-data request) :params))))
  (remove-route 'route-with-id))

(5am:def-test route-should-respond-when-dispatched ()
  (let ((rs (dispatch-route (wst.routing:make-request
                             :uri "/testing-route"
                             :method :GET))))
    (5am:is (equal "ok" (response-content rs)))))

(5am:def-test route-should-respond-with-default-not-found ()
  (let ((rs (dispatch-route (wst.routing:make-request
                             :uri "/x"
                             :method :GET))))
    (5am:is (= 404 (response-status rs)))
    (5am:is (equal "not found" (response-content rs)))))

(5am:def-test return-internal-server-error-if-exception-is-thrown ()

  (add-route 'throw-exception "/throw-exception" :GET (lambda (request response)
                                                        (declare (ignorable request response))
                                                        (error "something bad happened.")))
  (let ((rs (dispatch-route (wst.routing:make-request :uri "/throw-exception"
                                                      :method :GET))))
    (5am:is (equal "internal server error" (response-content rs)))
    (remove-route 'throw-exception)))

(defun route-responder (request response)
  (declare (ignore request response))
  (5am:is-true t))

(5am:def-test build-a-simple-route-using-the-dsl ()
  (setf wst.routing::*routes* nil)
  (wst.routing.dsl:build-webserver
   `(wst.routing.dsl:route :GET index "/" route-responder))
  (wst.routing:dispatch-route-by-name 'index (make-request :method :GET)))

(5am:def-test build-with-just-route-is-the-same-of-just-route-using-the-dsl ()
  (setf wst.routing::*routes* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :route (wst.routing.dsl:route :GET index "/" ,must-be-called)))
    (wst.routing:dispatch-route-by-name 'index (make-request :method :GET))
    (5am:is (= 1 count))))

(5am:def-test build-route-with-just-before ()
  (setf wst.routing::*routes* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,must-be-called
       :route (wst.routing.dsl:route :GET index "/" ,must-be-called)))
    (wst.routing:dispatch-route-by-name 'index (make-request :method :GET))
    (5am:is (= 2 count))))

(5am:def-test build-route-with-just-after ()
  (setf wst.routing::*routes* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :route (wst.routing.dsl:route :GET index "/" ,must-be-called)
       :after ,must-be-called))
    (wst.routing:dispatch-route-by-name 'index (make-request :method :GET))
    (5am:is (= 2 count))))

(5am:def-test build-a-route-wrapped-using-the-dsl ()
  (setf wst.routing::*routes* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,must-be-called
       :route (wst.routing.dsl:route :GET index "/" ,must-be-called)
       :after ,must-be-called))
    (wst.routing:dispatch-route-by-name 'index (make-request :method :GET))
    (5am:is (= 3 count))))

(5am:def-test build-group-of-routes-using-the-dsl ()
  (setf wst.routing::*routes* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:group
       (wst.routing.dsl:route :GET route-a "/a" ,must-be-called)
       (wst.routing.dsl:route :GET route-b "/b" ,must-be-called)))
    (wst.routing:dispatch-route-by-name 'route-a (make-request :method :GET))
    (wst.routing:dispatch-route-by-name 'route-b (make-request :method :GET))
    (5am:is (= 2 count))))

(5am:def-test build-a-resource-routes-using-the-dsl ()
  (setf wst.routing::*routes* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:resource "/base"
                                (wst.routing.dsl:route :GET route-a "/a" ,must-be-called)
                                (wst.routing.dsl:route :GET route-b ,must-be-called)))
    (wst.routing:dispatch-route (make-request :uri "/base/a" :method :GET))
    (wst.routing:dispatch-route (make-request :uri "/base" :method :GET))
    (5am:is (= 2 count))))

(5am:def-test build-an-any-route-using-the-dsl ()
  (setf wst.routing::*routes* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:any-route :GET ,must-be-called))
    (wst.routing:dispatch-route (make-request :uri "/mimimimimimimimi" :method :GET))
    (5am:is (= 1 count))))

(5am:def-test parse-request-cookies ()
  (add-route 'cookies "/" :GET (lambda (request response)
                                 (declare (ignorable response))
                                 (let ((cookies (getf (wst.routing:request-data request) :cookies)))
                                   (5am:is (= 2 (length (hu:hash-keys cookies))))
                                   response)))
  (dispatch-route (wst.routing:make-request
                   :uri "/"
                   :method :GET
                   :headers (cl-hash-util:hash ("cookie" "first=a; second=b"))))
  (remove-route 'cookies))

(5am:def-test customize-condition-handler ()
  (wst.routing:condition-handler
   (lambda (request response err)
     (declare (ignorable request))
     (5am:is (string-equal (simple-condition-format-control err)
                           "something went wrong."))
     (setf (wst.routing:response-data response)
           '("meh"))
     response))
  (add-route 'customize-condition-handler
             "/"
             :GET
             (lambda (request response)
               (declare (ignorable request response))
               (error "something went wrong.")))
  (let ((rs (dispatch-route (wst.routing:make-request
                             :uri "/"
                             :method :GET))))
    (5am:is (string-equal (car (response-data rs))
                          "meh"))
    (remove-route 'customize-condition-handler)
    (wst.routing:condition-handler nil)))

(5am:def-test unhandler-customized-condition-handler ()
  (wst.routing:condition-handler
   (lambda (request response err)
     (declare (ignorable request response err))))
  (add-route 'unhandler-customized-condition-handler
             "/"
             :GET
             (lambda (request response)
               (declare (ignorable request response))
               (error "something went wrong.")))
  (let ((rs (dispatch-route (wst.routing:make-request
                             :uri "/"
                             :method :GET))))
    (5am:is (= 500 (wst.routing:response-status rs)))
    (remove-route 'unhandler-customized-condition-handler)
    (wst.routing:condition-handler nil)))

(5am:def-test any-route-with-method ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (setf (response-content response) "ok")
                                        response))
  (let ((a (dispatch-route (wst.routing:make-request :uri "/a"
                                                     :method :GET)))
        (b (dispatch-route (wst.routing:make-request :uri "/b"
                                                     :method :GET))))
    (5am:is (equal (response-content a)
                   (response-content b)))
    (setf wst.routing::*any-route-handler* nil)))

(5am:def-test dispatch-by-name-any-route ()
  (let ((count 0))
    (wst.routing:any-route-handler :GET (lambda (request response)
                                          (declare (ignorable request))
                                          (setf count (1+ count))
                                          response))
    (dispatch-route-by-name 'a (wst.routing:make-request :uri "/a"
                                                         :method :GET))
    (dispatch-route-by-name 'b (wst.routing:make-request :uri "/b"
                                                         :method :GET))
    (5am:is (= count 2))
    (setf wst.routing::*any-route-handler* nil)))

(5am:def-test dont-dispatch-by-name-any-route-with-method-is-different ()
  (let ((count 0))
    (wst.routing:any-route-handler :GET (lambda (request response)
                                          (declare (ignorable request))
                                          (setf count (1+ count))
                                          response))
    (dispatch-route-by-name 'a (wst.routing:make-request :uri "/a"
                                                         :method :POST))
    (5am:is (= count 0))
    (setf wst.routing::*any-route-handler* nil)))

(5am:def-test respond-with-internal-server-error ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:internal-server-error-response t response)
                                        response))
  (5am:is (= 500 (wst.routing:response-status
                  (dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET)))))
  (setf wst.routing::*any-route-handler* nil))

(5am:def-test respond-with-unauthorized ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:unauthorized-response t response)
                                        response))
  (5am:is (= 401 (wst.routing:response-status
                  (dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET)))))
  (setf wst.routing::*any-route-handler* nil))

(5am:def-test respond-with-forbidden ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:forbidden-response t response)
                                        response))
  (5am:is (= 403 (wst.routing:response-status
                  (dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET)))))
  (setf wst.routing::*any-route-handler* nil))

(5am:def-test respond-with-bad-request ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:bad-request-response t response)
                                        response))
  (5am:is (= 400 (wst.routing:response-status
                  (dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET)))))
  (setf wst.routing::*any-route-handler* nil))

(5am:def-test respond-with-redirect-see-other ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:redirect-see-other-response t response "/redirect")
                                        response))
  (let ((rs (dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 303 (wst.routing:response-status rs)))
    (5am:is (string-equal (getf (wst.routing:response-headers rs) :location)
                          "/redirect"))
    (setf wst.routing::*any-route-handler* nil)))

(defmethod wst.routing:ok-response ((ty (eql :sexp)) response &key headers content)
  (declare (ignorable headers))
  (write-response response :status 200
                           :content-type "application/s-expression"
                           :headers headers
                           :content (format nil "~a" content)))

(5am:def-test respond-with-custom-responder ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (ok-response :sexp response :content (list 1 2 3))
                                        response))
  (let ((rs (dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 200 (wst.routing:response-status rs)))
    (5am:is (string-equal (getf (response-headers rs) :content-type)
                          "application/s-expression"))
    (5am:is (equal (response-content rs) "(1 2 3)"))
    (setf wst.routing::*any-route-handler* nil)))

(5am:def-test parse-request-uri-just-path ()
  (let ((uri "/a/b/c"))
    (multiple-value-bind (uri query-string hash)
        (wst.routing:parse-uri uri)
      (5am:is (string-equal uri "/a/b/c"))
      (5am:is (string-equal query-string ""))
      (5am:is (string-equal hash "")))))

(5am:def-test parse-request-uri-with-just-query ()
  (let ((uri "/a/b/c?ok=1"))
    (multiple-value-bind (uri query-string hash)
        (wst.routing:parse-uri uri)
      (5am:is (string-equal uri "/a/b/c"))
      (5am:is (string-equal query-string "ok=1"))
      (5am:is (string-equal hash "")))))

(5am:def-test parse-request-uri-with-just-hash ()
  (let ((uri "/a/b/c#anchor"))
    (multiple-value-bind (uri query-string hash)
        (wst.routing:parse-uri uri)
      (5am:is (string-equal uri "/a/b/c"))
      (5am:is (string-equal query-string ""))
      (5am:is (string-equal hash "anchor")))))

(5am:def-test parse-request-uri-with-query-and-hash ()
  (let ((uri "/a/b/c?ok=1#anchor"))
    (multiple-value-bind (uri query-string hash)
        (wst.routing:parse-uri uri)
      (5am:is (string-equal uri "/a/b/c"))
      (5am:is (string-equal query-string "ok=1"))
      (5am:is (string-equal hash "anchor")))))

(5am:def-test parse-request-root-uri-with-query-and-hash ()
  (let ((uri "/?ok=1#anchor"))
    (multiple-value-bind (uri query-string hash)
        (wst.routing:parse-uri uri)
      (5am:is (string-equal uri "/"))
      (5am:is (string-equal query-string "ok=1"))
      (5am:is (string-equal hash "anchor")))))
