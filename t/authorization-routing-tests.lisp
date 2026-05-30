(defpackage #:io.github.cl-sdk.wst.authorization.routing.test
  (:use #:cl #:fiveam
        #:io.github.cl-sdk.wst.authorization
        #:io.github.cl-sdk.wst.authorization.routing))

(in-package #:io.github.cl-sdk.wst.authorization.routing.test)

(def-suite authorization-routing-suite)
(in-suite authorization-routing-suite)

(test default-hooks-extract-subject-action-resource-and-context
  (let* ((reader (make-instance 'role :name :reader))
         (user (make-instance 'user :id "u1" :roles (list reader)))
         (session (create-session nil user))
         (request (io.github.cl-sdk.wst.routing:make-request :uri "/users" :method :get)))
    (setf (io.github.cl-sdk.wst.routing:request-data request)
          (append (io.github.cl-sdk.wst.routing:request-data request)
                  (list :session session :authorization-context '(:tenant "acme"))))
    (let ((context (authorization-context-of request)))
      (is (eq session (subject-of request)))
      (is (eq session (getf context :session)))
      (is (eq :get (action-of request)))
      (is (string= "/users" (resource-of request)))
      (is (string= "acme" (getf context :tenant))))))

(test wrap-authorization-halts-unauthenticated-requests-with-401
  (let* ((request (io.github.cl-sdk.wst.routing:make-request :uri "/users" :method :get))
         (response (io.github.cl-sdk.wst.routing:make-response))
         (middleware (wrap-authorization :policy (require-authenticated)))
         (result (funcall middleware request response)))
    (is (eq :halt (car result)))
    (is (= 401 (io.github.cl-sdk.wst.routing:response-status (cdr result))))
    (is (eq *decision-deny* (authorization-decision-of request)))))

(test wrap-authorization-allows-authorized-requests
  (let* ((permission (make-instance 'permission :operation :get :object "/users"))
         (reader (make-instance 'role :name :reader :permissions (list permission)))
         (user (make-instance 'user :id "u2" :roles (list reader)))
         (session (create-session nil user))
         (request (io.github.cl-sdk.wst.routing:make-request :uri "/users" :method :get))
         (response (io.github.cl-sdk.wst.routing:make-response))
         (middleware (wrap-authorization :policy (require-permission :get "/users"))))
    (setf (io.github.cl-sdk.wst.routing:request-data request)
          (append (io.github.cl-sdk.wst.routing:request-data request)
                  (list :session session)))
    (let ((result (funcall middleware request response)))
      (is (eq :continue (car result)))
      (is (eq *decision-allow* (authorization-decision-of request))))))

(test wrap-authorization-halts-authenticated-but-forbidden-requests-with-403
  (let* ((reader (make-instance 'role :name :reader))
         (user (make-instance 'user :id "u3" :roles (list reader)))
         (session (create-session nil user))
         (request (io.github.cl-sdk.wst.routing:make-request :uri "/users" :method :delete))
         (response (io.github.cl-sdk.wst.routing:make-response))
         (middleware (wrap-authorization :policy (require-permission :delete "/users"))))
    (setf (io.github.cl-sdk.wst.routing:request-data request)
          (append (io.github.cl-sdk.wst.routing:request-data request)
                  (list :session session)))
    (let ((result (funcall middleware request response)))
      (is (eq :halt (car result)))
      (is (= 403 (io.github.cl-sdk.wst.routing:response-status (cdr result))))
      (is (eq *decision-deny* (authorization-decision-of request))))))
