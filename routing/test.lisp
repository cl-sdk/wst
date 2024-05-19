;;; copyright (c) 2024 Bruno H. Dias <dias.h.bruno@gmail.com>

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
(5am:run! 'return-internal-server-error-if-exception-is-thrown)
