(defpackage #:io.github.cl-sdk.wst.feature-flag.routing
  (:use #:cl #:io.github.cl-sdk.wst.feature-flag)
  (:documentation "Routing adapter for request-scoped feature-flag contexts.")
  (:export
   #:wrap-feature-flag-context
   #:feature-flag-context-of
   #:client-for-request))

(in-package #:io.github.cl-sdk.wst.feature-flag.routing)

(defun wrap-feature-flag-context (&key
                                   (context-fn (lambda (request)
                                                 (declare (ignore request))
                                                 nil))
                                   (request-data-key :feature-flag-evaluation-context))
  "Create a before-middleware that injects request-scoped feature-flag context."
  (check-type request-data-key keyword)
  (lambda (request response)
    (let ((context (funcall context-fn request)))
      (setf (io.github.cl-sdk.wst.routing:request-data request)
            (append (io.github.cl-sdk.wst.routing:request-data request)
                    (list request-data-key
                          (merge-evaluation-contexts context))))
      (cons :continue response))))

(defun feature-flag-context-of (request &key (request-data-key :feature-flag-evaluation-context))
  "Return request-scoped feature-flag context from REQUEST, or NIL."
  (getf (io.github.cl-sdk.wst.routing:request-data request) request-data-key))

(defun client-for-request (request &key
                                     domain
                                     (name "request-client")
                                     evaluation-context
                                     (request-data-key :feature-flag-evaluation-context))
  "Create a feature-flag client composed with request-scoped context."
  (create-client :name name
                 :domain domain
                 :evaluation-context
                 (merge-evaluation-contexts
                  evaluation-context
                  (feature-flag-context-of request :request-data-key request-data-key))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (entry
           '((wrap-feature-flag-context
              . "Create middleware that writes request-scoped evaluation context.
Example:
  (wrap-feature-flag-context
   :context-fn (lambda (request)
                 (list :tenant (getf (io.github.cl-sdk.wst.routing:request-data request) :tenant))))
  => #<FUNCTION ...>")
             (feature-flag-context-of
              . "Read request-scoped feature-flag context from request-data.
Example:
  (feature-flag-context-of request)
  => (:tenant \"acme\" :plan \"pro\") or NIL")
             (client-for-request
              . "Create a client merged with request-scoped context.
Example:
  (client-for-request request :domain \"payments\" :evaluation-context '(:app \"checkout\"))
  => #S(FEATURE-FLAG-CLIENT ...)")))
    (setf (documentation (car entry) 'function) (cdr entry))))
