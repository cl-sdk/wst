(defpackage #:io.github.cl-sdk.wst.openfeature.routing.test
  (:use #:cl #:fiveam
        #:io.github.cl-sdk.wst.openfeature
        #:io.github.cl-sdk.wst.openfeature.routing))

(in-package #:io.github.cl-sdk.wst.openfeature.routing.test)

(def-suite openfeature-routing-suite)
(in-suite openfeature-routing-suite)

(defmacro with-openfeature-reset (&body body)
  `(unwind-protect
        (progn
          (reset-openfeature)
          ,@body)
     (reset-openfeature)))

(defclass context-capturing-provider (provider)
  ((last-context :accessor provider-last-context :initform nil)))

(defmethod resolve-boolean-details ((provider context-capturing-provider) flag-key default-value evaluation-context)
  (setf (provider-last-context provider) evaluation-context)
  (make-evaluation-details :flag-key flag-key
                           :value default-value
                           :reason *reason-static*))

(test middleware-injects-request-scoped-context
  (let* ((request (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))
         (response (io.github.cl-sdk.wst.routing:make-response))
         (middleware (wrap-openfeature-context
                      :context-fn (lambda (request)
                                    (declare (ignore request))
                                    '(:tenant "acme" :plan "pro"))))
         (result (funcall middleware request response)))
    (is (eq :continue (car result)))
    (is (equal '(:tenant "acme" :plan "pro")
               (openfeature-context-of request)))))

(test client-for-request-composes-request-scoped-context-into-client-context
  (with-openfeature-reset
    (let* ((provider (make-instance 'context-capturing-provider :name "capture"))
           (request (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))
           (response (io.github.cl-sdk.wst.routing:make-response))
           (middleware (wrap-openfeature-context
                        :context-fn (lambda (request)
                                      (declare (ignore request))
                                      '(:tenant "acme" :shared :request)))))
      (set-provider provider)
      (funcall middleware request response)
      (let ((client (client-for-request request :evaluation-context '(:shared :client :app "shop"))))
        (is-false (get-boolean-value client "flag-a" nil :evaluation-context '(:shared :call)))
        (let ((ctx (provider-last-context provider)))
          (is (string= "acme" (getf ctx :tenant)))
          (is (string= "shop" (getf ctx :app)))
          (is (eq :call (getf ctx :shared))))))))
