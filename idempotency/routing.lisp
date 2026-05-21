(defpackage #:io.github.cl-sdk.wst.idempotency.routing
  (:use #:cl #:io.github.cl-sdk.wst.idempotency)
  (:documentation "Routing middleware adapter for idempotency keys.")
  (:export
   #:idempotency-key
   #:idempotency-context-of))

(in-package #:io.github.cl-sdk.wst.idempotency.routing)

(defun %request-header-ci (request header-name)
  (let ((headers (io.github.cl-sdk.wst.routing:request-headers request)))
    (or (gethash header-name headers)
        (loop :for key :being :the :hash-keys :of headers
              :using (:hash-value value)
              :when (and (stringp key) (string-equal key header-name))
                :return value))))

(defun %default-scope-fn (request)
  (or (let ((route (getf (io.github.cl-sdk.wst.routing:request-data request) :route)))
        (and route (io.github.cl-sdk.wst.routing:route-path route)))
      (io.github.cl-sdk.wst.routing:request-uri request)))

(defun %default-fingerprint-fn (request scope)
  (make-fingerprint
   :method (io.github.cl-sdk.wst.routing:request-method request)
   :scope scope
   :body (io.github.cl-sdk.wst.routing:request-content request)))

(defun idempotency-context-of (request)
  "Return idempotency context injected by IDEMPOTENCY-KEY middleware."
  (getf (io.github.cl-sdk.wst.routing:request-data request) :idempotency-context))

(defun idempotency-key (&key
                          (methods '(:POST :PATCH))
                          (header-name "Idempotency-Key")
                          (require-key t)
                          (ttl-seconds 86400)
                          (scope-fn #'%default-scope-fn)
                          (fingerprint-fn #'%default-fingerprint-fn)
                          (in-progress-status 409)
                          (in-progress-content "request is already processing")
                          (conflict-status 409)
                          (conflict-content "idempotency key conflicts with a different request")
                          (missing-key-status 400)
                          (missing-key-content "missing idempotency key")
                          (engine nil)
                          (lifecycle (idempotency :ttl-seconds ttl-seconds :engine engine)))
  "Create idempotency middleware pair for `wst.routing.dsl:build-webserver`.

Returns:
- :BEFORE middleware that enforces idempotency key policy
- :AFTER middleware that persists replayable responses"
  (labels ((protected-method-p (request)
             (member (io.github.cl-sdk.wst.routing:request-method request)
                     methods
                     :test #'eql))
           (replay-response (response cached)
             (setf (io.github.cl-sdk.wst.routing:response-status response)
                   (cached-response-status cached)
                   (io.github.cl-sdk.wst.routing:response-headers response)
                   (append (copy-list (cached-response-headers cached))
                           (list :idempotency-replayed "true"))
                   (io.github.cl-sdk.wst.routing:response-content response)
                   (cached-response-content cached))
             response)
           (deny-response (response status content)
             (io.github.cl-sdk.wst.routing:write-response response status nil content)
             response)
           (response->cached (response)
             (make-cached-response
              :status (io.github.cl-sdk.wst.routing:response-status response)
              :headers (copy-list (io.github.cl-sdk.wst.routing:response-headers response))
              :content (io.github.cl-sdk.wst.routing:response-content response))))
    (list
     :before
     (lambda (request response)
       (if (not (protected-method-p request))
           (cons :continue response)
             (let* ((raw-key (%request-header-ci request header-name))
                    (key (and raw-key
                              (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw-key)))
                                (unless (string= "" trimmed)
                                  trimmed)))))
               (cond
                 ((and require-key (not key))
                  (cons :halt (deny-response response missing-key-status missing-key-content)))
                 ((not key)
                  (cons :continue response))
                (t
                 (let* ((scope (funcall scope-fn request))
                        (fingerprint (funcall fingerprint-fn request scope)))
                   (multiple-value-bind (decision replayed)
                       (funcall lifecycle :begin scope key fingerprint)
                     (ecase decision
                      (:started
                       (io.github.cl-sdk.wst.routing:append-request-data
                        request
                        :idempotency-context
                        (list :scope scope
                              :key key
                              :fingerprint fingerprint))
                       (cons :continue response))
                      (:replay
                       (cons :halt (replay-response response replayed)))
                      (:in-progress
                       (cons :halt (deny-response response in-progress-status in-progress-content)))
                      (:conflict
                       (cons :halt (deny-response response conflict-status conflict-content)))))))))))
      :after
      (lambda (request response)
        (alexandria:when-let ((ctx (idempotency-context-of request)))
          (funcall lifecycle
                   :finish
                   (getf ctx :scope)
                   (getf ctx :key)
                   (getf ctx :fingerprint)
                   (response->cached response))
          (io.github.cl-sdk.wst.routing:remove-request-data request :idempotency-context))
        response))))
