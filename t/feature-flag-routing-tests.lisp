(defpackage #:io.github.cl-sdk.wst.feature-flag.routing.test
  (:use #:cl #:fiveam
        #:io.github.cl-sdk.wst.feature-flag
        #:io.github.cl-sdk.wst.feature-flag.routing))

(in-package #:io.github.cl-sdk.wst.feature-flag.routing.test)

(def-suite feature-flag-routing-suite)
(in-suite feature-flag-routing-suite)

(defclass context-capturing-provider (provider)
  ((last-context :accessor provider-last-context :initform nil)))

(defmethod resolve-boolean-details ((provider context-capturing-provider) flag-key default-value evaluation-context)
  (setf (provider-last-context provider) evaluation-context)
  (make-evaluation-details :flag-key flag-key
                           :value default-value
                           :reason *reason-static*))

(defmethod resolve-provider ((request io.github.cl-sdk.wst.routing::request) domain)
  (declare (ignore domain))
  (or (getf (io.github.cl-sdk.wst.routing:request-data request) :feature-flag-provider)
     (call-next-method)))

(defmethod acquire-client ((provider context-capturing-provider) &key (domain "default"))
  (make-instance 'client :provider provider :domain domain))

(test middleware-injects-request-scoped-context
  (let* ((request (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))
         (response (io.github.cl-sdk.wst.routing:make-response))
         (middleware (wrap-feature-flag-context
                      :context-fn (lambda (request)
                                    (declare (ignore request))
                                    '(:tenant "acme" :plan "pro"))))
         (result (funcall middleware request response)))
    (is (eq :continue (car result)))
    (let ((context (feature-flag-context-of request)))
      (is (string= "acme" (getf context :tenant)))
      (is (string= "pro" (getf context :plan))))))

(test client-for-request-composes-request-scoped-context-into-client-context
  (let ((provider (make-instance 'context-capturing-provider :domain "capture"))
        (request (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET))
        (response (io.github.cl-sdk.wst.routing:make-response))
        (middleware (wrap-feature-flag-context
                     :context-fn (lambda (request)
                                   (declare (ignore request))
                                   '(:tenant "acme" :shared :request)))))
    (setf (io.github.cl-sdk.wst.routing:request-data request)
          (append (io.github.cl-sdk.wst.routing:request-data request)
                  (list :feature-flag-provider provider)))
    (funcall middleware request response)
    (let ((client (client-for-request request :evaluation-context '(:shared :client :app "shop"))))
      (is-false (get-boolean-value client "flag-a" nil :evaluation-context '(:shared :call)))
      (let ((ctx (provider-last-context provider)))
        (is (string= "acme" (getf ctx :tenant)))
        (is (string= "shop" (getf ctx :app)))
        (is (eq :call (getf ctx :shared)))))))
