(defpackage #:wst.routing.test
  (:use #:cl))

(in-package :wst.routing.test)

(5am:def-suite wst.routing.suite)

(5am:in-suite wst.routing.suite)

(defmacro def-route-testing (name args &body body)
  (declare (ignorable args))
  `(5am:def-test ,name ()
     ,@body
     (setf wst.routing::*routes* nil
           wst.routing::*condition-handler* nil
           wst.routing::*any-route-handler* nil)))

(wst.routing:route test-route :GET "/testing-route" (request response)
  (declare (ignorable request))
  (wst.routing:ok-response t response :content "ok"))

(def-route-testing route-should-respond-when-dispatched ()
  (5am:is-true (fboundp 'test-route))
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/testing-route"
                                                                  :method :GET))))
    (5am:is (equal "ok" (wst.routing:response-content rs)))))

(def-route-testing route-should-respond-with-404-when-dispatched-with-wrong-method ()
  (let ((response (wst.routing:dispatch-route (wst.routing:make-request :uri "/"
                                                                        :method :POST))))
    (5am:is (equal "not found" (wst.routing:response-content response)))))

(def-route-testing removing-test-route ()
  (wst.routing:add-route 'to-be-remove "/to-be-removed" :GET (lambda (a b) (declare (ignorable a b)) t))
  (wst.routing:remove-route 'to-be-remove)
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/to-be-removed"
                                                                  :method :GET))))
    (5am:is (equal "not found" (wst.routing:response-content rs)))))

(def-route-testing allow-parameters-on-path ()
  (wst.routing:add-route 'route-with-id
                         "/r/:id"
                         :GET
                         (lambda (request r)
                           (declare (ignore r))
                           (wst.routing:with-request-data (params)
                               request
                             (5am:is (equalp '(("id" . "6")) params)))))
  (wst.routing:dispatch-route
   (wst.routing:make-request :uri "/r/6" :method :GET))
  (wst.routing:remove-route 'route-with-id))

(def-route-testing route-should-respond-with-default-not-found ()
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request
                                         :uri "/x"
                                         :method :GET))))
    (5am:is (= 404 (wst.routing:response-status rs)))
    (5am:is (equal "not found" (wst.routing:response-content rs)))))

(def-route-testing return-internal-server-error-if-exception-is-thrown ()
  (wst.routing:add-route 'throw-exception "/throw-exception" :GET (lambda (request response)
                                                                    (declare (ignorable request response))
                                                                    (error "something bad happened.")))
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request :uri "/throw-exception"
                                                                  :method :GET))))
    (5am:is (equal "internal server error" (wst.routing:response-content rs)))))

(defun route-responder (request response)
  (declare (ignore request response))
  (5am:is-true t))

(def-route-testing build-a-simple-route-using-the-dsl ()
  (wst.routing.dsl:build-webserver
   `(wst.routing.dsl:route :GET index "/" route-responder))
  (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET)))

(def-route-testing build-with-just-route-is-the-same-of-just-route-using-the-dsl ()
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :route (wst.routing.dsl:route :GET index "/" ,must-be-called)))
    (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET))
    (5am:is (= 1 count))))

(def-route-testing build-route-with-just-before ()
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,must-be-called
       :route (wst.routing.dsl:route :GET index "/" ,must-be-called)))
    (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-route-with-just-after ()
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :route (wst.routing.dsl:route :GET index "/" ,must-be-called)
       :after ,must-be-called))
    (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-a-route-wrapped-using-the-dsl ()
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,must-be-called
       :route (wst.routing.dsl:route :GET index "/" ,must-be-called)
       :after ,must-be-called))
    (wst.routing:dispatch-route-by-name 'index (wst.routing:make-request :method :GET))
    (5am:is (= 3 count))))

(def-route-testing build-group-of-routes-using-the-dsl ()
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:group
       (wst.routing.dsl:route :GET route-a "/a" ,must-be-called)
       (wst.routing.dsl:route :GET route-b "/b" ,must-be-called)))
    (wst.routing:dispatch-route-by-name 'route-a (wst.routing:make-request :method :GET))
    (wst.routing:dispatch-route-by-name 'route-b (wst.routing:make-request :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-a-resource-routes-using-the-dsl ()
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:resource "/base"
                                (wst.routing.dsl:route :GET route-a "/a" ,must-be-called)
                                (wst.routing.dsl:route :GET route-b ,must-be-called)))
    (wst.routing:dispatch-route (wst.routing:make-request :uri "/base/a" :method :GET))
    (wst.routing:dispatch-route (wst.routing:make-request :uri "/base" :method :GET))
    (5am:is (= 2 count))))

(def-route-testing build-an-any-route-using-the-dsl ()
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:any-route :GET ,must-be-called))
    (wst.routing:dispatch-route (wst.routing:make-request :uri "/mimimimimimimimi" :method :GET))
    (5am:is (= 1 count))))

(def-route-testing build-a-route-using-the-dsl-with-custom-data ()
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:group
       (wst.routing.dsl:route :GET route-a "/" ,must-be-called :custom :ok)
       (wst.routing.dsl:resource "/a"
                                 (wst.routing.dsl:route :GET route-b ,must-be-called :custom :ok))))
    (5am:is-true (equal :ok (car (wst.routing::route-custom (wst.routing:find-route-by-name 'route-a)))))
    (5am:is-true (equal :ok (car (wst.routing::route-custom (wst.routing:find-route-by-name 'route-b)))))))

(def-route-testing parse-request-cookies ()
  (wst.routing:add-route 'cookies "/" :GET (lambda (request response)
                                             (declare (ignorable response))
                                             (let ((cookies (getf (wst.routing:request-data request) :cookies)))
                                               (5am:is (= 2 (length (cl-hash-util:hash-keys cookies))))
                                               response)))
  (wst.routing:dispatch-route (wst.routing:make-request
                               :uri "/"
                               :method :GET
                               :headers (cl-hash-util:hash ("cookie" "first=a; second=b")))))

(def-route-testing customize-condition-handler ()
  (wst.routing:condition-handler
   (lambda (request response err)
     (declare (ignorable request))
     (5am:is (string-equal (simple-condition-format-control err)
                           "something went wrong."))
     (setf (wst.routing:response-data response)
           '("meh"))
     response))
  (wst.routing:add-route 'customize-condition-handler
                         "/"
                         :GET
                         (lambda (request response)
                           (declare (ignorable request response))
                           (error "something went wrong.")))
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request
                                         :uri "/"
                                         :method :GET))))
    (5am:is (string-equal (car (wst.routing:response-data rs))
                          "meh"))
    (wst.routing:remove-route 'customize-condition-handler)
    (wst.routing:condition-handler nil)))

(def-route-testing unhandler-customized-condition-handler ()
  (wst.routing:condition-handler
   (lambda (request response err)
     (declare (ignorable request response err))))
  (wst.routing:add-route 'unhandler-customized-condition-handler
                         "/"
                         :GET
                         (lambda (request response)
                           (declare (ignorable request response))
                           (error "something went wrong.")))
  (let ((rs (wst.routing:dispatch-route (wst.routing:make-request
                                         :uri "/"
                                         :method :GET))))
    (5am:is (= 500 (wst.routing:response-status rs)))
    (wst.routing:remove-route 'unhandler-customized-condition-handler)
    (wst.routing:condition-handler nil)))

(def-route-testing any-route-with-method ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (setf (wst.routing:response-content response) "ok")
                                        response))
  (let ((a (wst.routing:dispatch-route (wst.routing:make-request :uri "/a"
                                                                 :method :GET)))
        (b (wst.routing:dispatch-route (wst.routing:make-request :uri "/b"
                                                                 :method :GET))))
    (5am:is (equal (wst.routing:response-content a)
                   (wst.routing:response-content b)))))

(def-route-testing dispatch-by-name-any-route ()
  (let ((count 0))
    (wst.routing:any-route-handler :GET (lambda (request response)
                                          (declare (ignorable request))
                                          (setf count (1+ count))
                                          response))
    (wst.routing:dispatch-route-by-name 'a (wst.routing:make-request :uri "/a"
                                                                     :method :GET))
    (wst.routing:dispatch-route-by-name 'b (wst.routing:make-request :uri "/b"
                                                                     :method :GET))
    (5am:is (= count 2))))

(def-route-testing dont-dispatch-by-name-any-route-with-method-is-different ()
  (let ((count 0))
    (wst.routing:any-route-handler :GET (lambda (request response)
                                          (declare (ignorable request))
                                          (setf count (1+ count))
                                          response))
    (wst.routing:dispatch-route-by-name
     'a
     (wst.routing:make-request :uri "/a" :method :POST))
    (5am:is (= count 0))))

(def-route-testing respond-with-internal-server-error ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:internal-server-error-response t response)
                                        response))
  (5am:is (= 500 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a
                   (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-unauthorized ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:unauthorized-response t response)
                                        response))
  (5am:is (= 401 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a
                   (wst.routing:make-request :uri "/" :method :GET))))))

(5am:def-test respond-with-forbidden ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:forbidden-response t response)
                                        response))
  (5am:is (= 403 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name
                   'a
                   (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-bad-request ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:bad-request-response t response)
                                        response))
  (5am:is (= 400 (wst.routing:response-status
                  (wst.routing:dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET))))))

(def-route-testing respond-with-redirect-see-other ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:redirect-see-other-response t response "/redirect")
                                        response))
  (let ((rs (wst.routing:dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 303 (wst.routing:response-status rs)))
    (5am:is (string-equal (getf (wst.routing:response-headers rs) :location)
                          "/redirect"))))

(defmethod wst.routing:ok-response ((ty (eql :sexp)) response &key headers content)
  (declare (ignorable headers))
  (wst.routing:write-response response :status 200
                                       :content-type "application/s-expression"
                                       :headers headers
                                       :content (format nil "~a" content)))

(def-route-testing respond-with-custom-responder ()
  (wst.routing:any-route-handler :GET (lambda (request response)
                                        (declare (ignorable request))
                                        (wst.routing:ok-response :sexp response :content (list 1 2 3))
                                        response))
  (let ((rs (wst.routing:dispatch-route-by-name 'a (wst.routing:make-request :uri "/" :method :GET))))
    (5am:is (= 200 (wst.routing:response-status rs)))
    (5am:is (string-equal (getf (wst.routing:response-headers rs) :content-type)
                          "application/s-expression"))
    (5am:is (equal (wst.routing:response-content rs) "(1 2 3)"))))

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
