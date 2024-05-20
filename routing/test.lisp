(defpackage #:wst.routing.test
  (:use #:cl)
  (:import-from #:cl-hash-util
                #:hash-create)
  (:import-from #:wst.routing
                #:add-route
                #:remove-route
                #:dispatch-route
                #:response
                #:route))

(in-package #:wst.routing.test)

(5am:def-suite wst.routing.suite)

(5am:in-suite wst.routing.suite)

(route test-route :get "/testing-route" (request params)
  (response t "ok"))

(5am:def-test test-route-was-compiled ()
  (5am:is-true (fboundp 'test-route)))

(5am:def-test test-route-should-respond-when-dispatched ()
  (5am:is (equal
           "ok"
           (dispatch-route "/testing-route" :get '(:request-method :get)))))

(5am:def-test test-route-should-respond-with-404-when-dispatched-with-wrong-method ()
  (let ((response (dispatch-route "/aaaaa" :get '(:request-method :post))))
    (5am:is (equal "not found" response))))

(5am:def-test removing-test-route ()
  (add-route 'to-be-remove "/to-be-removed" :get (lambda (a b) t))
  (remove-route 'to-be-remove)
  (5am:is (equal
           "not found"
           (dispatch-route "/to-be-removed" :get nil))))

(5am:def-test allow-parameters-on-path ()
  (add-route 'route-with-id "/r/:id" :get (lambda (request params)
                                            (declare (ignore request))
                                            params))
  (let ((params (dispatch-route "/r/6" :get nil)))
    (5am:is (equalp '(("id" . "6")) params)))
  (remove-route 'route-with-id))

(5am:def-test return-internal-server-error-if-exception-is-thrown ()
  (add-route 'throw-exception "/throw-exception" :get (lambda (request params)
                                                        (declare (ignorable request params))
                                                        (error "something bad happened.")))
  (5am:is (equal "internal server error"
                 (dispatch-route "/throw-exception" :get nil)))
  (remove-route 'throw-exception))

(5am:def-test build-a-simple-route-using-the-dsl ()
  (setf wst.routing::*route-specs* nil)
  (flet ((the-function ()
           (5am:is-true t)))
    (wst.routing.dsl:build-webserver
     '(wst.routing.dsl:route :get index "/" the-function))
    (wst.routing:dispatch-route-by-name 'index '(:request-method :get))))

(5am:def-test build-with-just-route-is-the-same-of-just-route-using-the-dsl ()
  (setf wst.routing::*route-specs* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :route (wst.routing.dsl:route :get index "/" ,must-be-called)))
    (wst.routing:dispatch-route-by-name 'index '(:request-method :get))
    (5am:is (= 1 count))))

(5am:def-test build-route-with-just-before ()
  (setf wst.routing::*route-specs* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,must-be-called
       :route (wst.routing.dsl:route :get index "/" ,must-be-called)))
    (wst.routing:dispatch-route-by-name 'index '(:request-method :get))
    (5am:is (= 2 count))))

(5am:def-test build-route-with-just-after ()
  (setf wst.routing::*route-specs* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :route (wst.routing.dsl:route :get index "/" ,must-be-called)
       :after ,must-be-called))
    (wst.routing:dispatch-route-by-name 'index '(:request-method :get))
    (5am:is (= 2 count))))

(5am:def-test build-a-route-wrapped-using-the-dsl ()
  (setf wst.routing::*route-specs* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:wrap
       :before ,must-be-called
       :route (wst.routing.dsl:route :get index "/" ,must-be-called)
       :after ,must-be-called))
    (wst.routing:dispatch-route-by-name 'index '(:request-method :get))
    (5am:is (= 3 count))))

(5am:def-test build-group-of-routes-using-the-dsl ()
  (setf wst.routing::*route-specs* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:group
       (wst.routing.dsl:route :get route-a "/a" ,must-be-called)
       (wst.routing.dsl:route :get route-b "/b" ,must-be-called)))
    (wst.routing:dispatch-route-by-name 'route-a '(:request-method :get))
    (wst.routing:dispatch-route-by-name 'route-b '(:request-method :get))
    (5am:is (= 2 count))))

(5am:def-test build-a-resource-routes-using-the-dsl ()
  (setf wst.routing::*route-specs* nil)
  (let* ((count 0)
         (must-be-called (lambda (req res)
                           (declare (ignore req res))
                           (setf count (1+ count)))))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:resource "/base"
       (wst.routing.dsl:route :get route-a "/a" ,must-be-called)
       (wst.routing.dsl:route :get route-b ,must-be-called)))
    (wst.routing:dispatch-route "/base/a" :get '(:request-method :get))
    (wst.routing:dispatch-route "/base" :get '(:request-method :get))
    (5am:is (= 2 count))))
