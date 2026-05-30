(defpackage #:io.github.cl-sdk.wst.authorization.test
  (:use #:cl #:fiveam #:io.github.cl-sdk.wst.authorization))

(in-package #:io.github.cl-sdk.wst.authorization.test)

(def-suite authorization-suite)
(in-suite authorization-suite)

(defun account-active-p (context)
  (getf context :account-active))

(defpolicy can-read-users
  (all-of (require-authenticated)
          (any-of (require-role :admin)
                  (require-role :reader))))

(defpolicy-set api-policies
  :combiner deny-overrides
  :policies (list can-read-users
                  (require-predicate #'account-active-p)))

(test create-session-manages-active-roles
  (let* ((reader (make-instance 'role :name :reader))
         (admin (make-instance 'role :name :admin))
         (user (make-instance 'user :id "u1" :roles (list reader admin)))
         (session (create-session nil user :roles (list reader))))
    (is (= 1 (length (active-roles session))))
    (is (eq reader (car (active-roles session))))
    (activate-role nil session admin)
    (is (= 2 (length (active-roles session))))
    (deactivate-role nil session reader)
    (is (= 1 (length (active-roles session))))
    (is (eq admin (car (active-roles session))))))

(test session-authorization-checks-permissions-and-inherited-roles
  (let* ((read-users (make-instance 'permission :operation :get :object "/users"))
         (write-users (make-instance 'permission :operation :post :object "/users"))
         (reader (make-instance 'role :name :reader :permissions (list read-users)))
         (admin (make-instance 'role :name :admin
                               :permissions (list write-users)
                               :seniors (list reader)))
         (user (make-instance 'user :id "u2" :roles (list admin)))
         (session (create-session nil user)))
    (is-true (session-authorized-p nil session :get "/users"))
    (is-true (session-authorized-p nil session :post "/users"))
    (is-false (session-authorized-p nil session :delete "/users"))))

(test primitive-policies-and-combinators-produce-expected-decisions
  (let* ((permission (make-instance 'permission :operation :get :object "/users"))
         (reader (make-instance 'role :name :reader :permissions (list permission)))
         (user (make-instance 'user :id "u3" :roles (list reader)))
         (session (create-session nil user))
         (context (list :session session)))
    (is (eq *decision-allow*
            (evaluate-policy (require-authenticated) context)))
    (is (eq *decision-deny*
            (evaluate-policy (require-authenticated) nil)))
    (is (eq *decision-allow*
            (evaluate-policy (require-role :reader) context)))
    (is (eq *decision-deny*
            (evaluate-policy (require-role :admin) context)))
    (is (eq *decision-allow*
            (evaluate-policy (require-permission :get "/users") context)))
    (is (eq *decision-deny*
            (evaluate-policy (require-permission :delete "/users") context)))
    (is (eq *decision-allow*
            (evaluate-policy (all-of (require-authenticated)
                                     (require-role :reader))
                             context)))
    (is (eq *decision-deny*
            (evaluate-policy (any-of (require-role :admin)
                                     (require-predicate (lambda (_context)
                                                          (declare (ignore _context))
                                                          nil)))
                             context)))
    (is (eq *decision-allow*
            (evaluate-policy (invert (require-role :admin)) context)))
    (is (eq *decision-deny*
            (evaluate-policy (deny-overrides
                              (require-predicate (lambda (_context)
                                                   (declare (ignore _context))
                                                   *decision-not-applicable*))
                              (require-role :admin))
                             context)))
    (is (eq *decision-allow*
            (evaluate-policy (permit-overrides
                              (require-role :admin)
                              (require-role :reader))
                             context)))
    (is (eq *decision-allow*
            (evaluate-policy (first-applicable
                              (require-predicate (lambda (_context)
                                                   (declare (ignore _context))
                                                   *decision-not-applicable*))
                              (require-role :reader)
                              (require-role :admin))
                             context)))))

(test dsl-macros-build-composable-policies
  (let* ((reader (make-instance 'role :name :reader))
         (user (make-instance 'user :id "u4" :roles (list reader)))
         (session (create-session nil user))
         (context (list :session session :account-active t)))
    (is (eq *decision-allow*
            (evaluate-policy can-read-users context)))
    (is (eq *decision-allow*
            (evaluate-policy api-policies context)))
    (is (eq *decision-deny*
            (evaluate-policy api-policies (list :session session :account-active nil))))))
