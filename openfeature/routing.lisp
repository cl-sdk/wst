(defpackage #:io.github.cl-sdk.wst.openfeature.routing
  (:use #:cl #:io.github.cl-sdk.wst.openfeature)
  (:documentation "Routing adapter for request-scoped OpenFeature contexts.")
  (:export
   #:wrap-openfeature-context
   #:openfeature-context-of
   #:client-for-request))

(in-package #:io.github.cl-sdk.wst.openfeature.routing)

(defun wrap-openfeature-context (&key
                                   (context-fn (lambda (request)
                                                 (declare (ignore request))
                                                 nil))
                                   (request-data-key :openfeature-evaluation-context))
  "Create a before-middleware that injects request-scoped OpenFeature context."
  (check-type request-data-key keyword)
  (lambda (request response)
    (let ((context (funcall context-fn request)))
      (setf (io.github.cl-sdk.wst.routing:request-data request)
            (append (io.github.cl-sdk.wst.routing:request-data request)
                    (list request-data-key
                          (merge-evaluation-contexts context))))
      (cons :continue response))))

(defun openfeature-context-of (request &key (request-data-key :openfeature-evaluation-context))
  "Return request-scoped OpenFeature context from REQUEST, or NIL."
  (getf (io.github.cl-sdk.wst.routing:request-data request) request-data-key))

(defun client-for-request (request &key
                                     domain
                                     (name "request-client")
                                     evaluation-context
                                     (request-data-key :openfeature-evaluation-context))
  "Create an OpenFeature client composed with request-scoped context."
  (create-client :name name
                 :domain domain
                 :evaluation-context
                 (merge-evaluation-contexts
                  evaluation-context
                  (openfeature-context-of request :request-data-key request-data-key))))
