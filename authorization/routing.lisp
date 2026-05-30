(defpackage #:io.github.cl-sdk.wst.authorization.routing
  (:use #:cl #:io.github.cl-sdk.wst.authorization)
  (:documentation "Routing adapter for request-scoped authorization checks.")
  (:export
   #:wrap-authorization
   #:subject-of
   #:resource-of
   #:action-of
   #:authorization-context-of
   #:authorization-decision-of))

(in-package #:io.github.cl-sdk.wst.authorization.routing)

(defun %plist-even-p (plist)
  (and (listp plist)
       (evenp (length plist))))

(defun %merge-contexts (&rest contexts)
  (let ((result nil))
    (dolist (context contexts result)
      (when context
        (unless (%plist-even-p context)
          (error "Authorization context must be a plist (even-length list)."))
        (loop :for (key value) :on context :by #'cddr
              :do (setf (getf result key) value))))))

(defun subject-of (request &key
                           (subject-key :subject)
                           (session-key :session))
  "Return the default authorization subject associated with REQUEST."
  (or (getf (io.github.cl-sdk.wst.routing:request-data request) subject-key)
      (getf (io.github.cl-sdk.wst.routing:request-data request) session-key)))

(defun action-of (request)
  "Return the default action associated with REQUEST."
  (io.github.cl-sdk.wst.routing:request-method request))

(defun resource-of (request)
  "Return the default resource associated with REQUEST."
  (io.github.cl-sdk.wst.routing:request-uri request))

(defun authorization-context-of (request &key
                                         (context-key :authorization-context)
                                         (subject-key :subject)
                                         (session-key :session)
                                         (store-key :authorization-store))
  "Return the default authorization evaluation context for REQUEST."
  (let* ((data (io.github.cl-sdk.wst.routing:request-data request))
         (subject (subject-of request :subject-key subject-key :session-key session-key))
         (session (or (and (typep subject 'session) subject)
                      (getf data session-key)))
         (action (action-of request))
         (resource (resource-of request)))
    (%merge-contexts
     (getf data context-key)
     (list :request request
           :subject subject
           :session session
           :store (or (getf data store-key)
                      (and session (session-store session)))
           :action action
           :operation action
           :resource resource
           :object resource))))

(defun authorization-decision-of (request &key (decision-key :authorization-decision))
  "Return the last authorization decision stored in REQUEST."
  (getf (io.github.cl-sdk.wst.routing:request-data request) decision-key))

(defun %default-on-deny (request response decision context)
  (declare (ignore request decision))
  (if (getf context :session)
      (cons :halt (io.github.cl-sdk.wst.routing:forbidden-response t response))
      (cons :halt (io.github.cl-sdk.wst.routing:unauthorized-response t response))))

(defun wrap-authorization (&key
                           policy
                           (context-fn #'authorization-context-of)
                           (on-deny #'%default-on-deny)
                           (decision-key :authorization-decision))
  "Create a before-middleware that evaluates POLICY for each request."
  (unless policy
    (error "wrap-authorization requires a policy."))
  (check-type decision-key keyword)
  (lambda (request response)
    (let* ((context (funcall context-fn request))
           (decision (evaluate-policy policy context)))
      (setf (io.github.cl-sdk.wst.routing:request-data request)
            (append (io.github.cl-sdk.wst.routing:request-data request)
                    (list decision-key decision)))
      (if (eq decision *decision-allow*)
          (cons :continue response)
          (funcall on-deny request response decision context)))))
