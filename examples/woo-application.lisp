(ql:quickload '(:wst.routing
                :wst.routing.dsl
                :wst.routing.response.dsl
                :wst.routing.woo
                :wst.session.csrf
                :wst.request-content
                :wst.request-content.routing
                :wst.cookies
                :wst.rate-limit
                :wst.circuit-breaker.routing
                :woo))

(defpackage #:wst.example.woo-application
  (:use #:cl))

(in-package #:wst.example.woo-application)

(defparameter *circuit-breaker*
  (wst.circuit-breaker.routing:circuit-breaker
   :failure-threshold 2
   :recovery-timeout 30))

(defparameter *rate-limiter*
  (wst.rate-limit:rate-limit :max-requests 5 :window-seconds 30))

(defparameter *parse-content-middleware*
  (wst.request-content.routing:parse-request-content))

(defclass csrf-store ()
  ((tokens
    :initform (make-hash-table :test 'equal)
    :accessor csrf-store-tokens)))

(defparameter *csrf-store* (make-instance 'csrf-store))

(defun request-client-id (request)
  (or (gethash "x-forwarded-for" (wst.routing:request-headers request))
      "global"))

(defun generate-csrf-token ()
  (format nil "~36R~36R"
          (random most-positive-fixnum)
          (get-universal-time)))

(defmethod wst.session.csrf:session-csrf-token ((store csrf-store) &key client-id)
  (gethash client-id (csrf-store-tokens store)))

(defmethod wst.session.csrf:add-session-csrf-token ((store csrf-store) key &key client-id)
  (setf (gethash client-id (csrf-store-tokens store)) key))

(defmethod wst.session.csrf:remove-session-csrf-token ((store csrf-store) &key client-id)
  (remhash client-id (csrf-store-tokens store)))

(defmethod wst.session.csrf:verify-session-csrf-token ((store csrf-store) key &key client-id)
  (let ((current (wst.session.csrf:session-csrf-token store :client-id client-id)))
    (and current (string= current key))))

(defun csrf-token-handler (request response)
  (let* ((client-id (request-client-id request))
         (token (generate-csrf-token)))
    (wst.session.csrf:add-session-csrf-token *csrf-store* token :client-id client-id)
    (wst.routing:ok-response t response
                             :headers (list :x-csrf-token token)
                             :content token)))

(defun csrf-before (request response)
  (let ((method (wst.routing:request-method request)))
    (if (member method '(:POST :PUT :PATCH :DELETE))
        (let ((token (gethash "x-csrf-token" (wst.routing:request-headers request))))
          (if (wst.session.csrf:verify-session-csrf-token
               *csrf-store*
               token
               :client-id (request-client-id request))
              (cons :continue response)
              (cons :halt (wst.routing:forbidden-response t response :content "invalid csrf token"))))
        (cons :continue response))))

(defun rate-limit-before (request response)
  (multiple-value-bind (allowed-p retry-after)
      (funcall *rate-limiter*
               (or (gethash "x-forwarded-for" (wst.routing:request-headers request))
                   "global"))
    (if allowed-p
        (cons :continue response)
        (cons :halt
              (wst.routing:too-many-requests-response
               t response
               :headers (list :retry-after (format nil "~a" retry-after)))))))

(defun index-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response t response :content "wst + woo example app"))

(defun health-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response t response :content "ok"))

(defun app-condition-handler (request response err)
  (let ((message (format nil "condition handled~%method: ~a~%uri: ~a~%type: ~a~%message: ~a"
                         (wst.routing:request-method request)
                         (wst.routing:request-uri request)
                         (type-of err)
                         err)))
    (format *error-output* "~&~a~%" message)
    (wst.routing:internal-server-error-response
     t response
     :content message)))

(defun users-handler (request response)
  (declare (ignore request))
  (wst.routing.response.dsl:status 200 response)
  (wst.routing.response.dsl:json t "{\"users\":[{\"id\":1,\"name\":\"alice\"}]}" response))

(defmethod wst.request-content:parse-content
    ((type (eql :|application/json|)) content &optional (encoding :utf-8))
  (declare (ignore type))
  (com.inuoe.jzon:parse (wst.request-content:content-as-string content encoding)))

(defun echo-handler (request response)
  (let ((body (wst.routing:request-content request)))
    (wst.routing:ok-response t response :content (format nil "~a" body))))

(defun cookies-handler (request response)
  (let ((cookies (wst.cookies:parse-cookies (wst.routing:request-headers request))))
    (wst.routing:ok-response t response
                             :content (format nil "cookies=~a" (length cookies)))))

(defun flaky-handler (request response)
  (let ((query (or (wst.routing:request-query request) "")))
    (if (zerop (length query))
        (wst.routing:ok-response t response :content "stable response")
        (let* ((parsed-query (wst.request-content:parse-content :|application/x-www-form-urlencoded| query))
               (fail (cdr (assoc "fail" parsed-query :test #'string=))))
          (if (and fail (string-equal fail "true"))
              (wst.routing:internal-server-error-response t response :content "forced failure")
               (wst.routing:ok-response t response :content "stable response"))))))

(defun boom-handler (request response)
  (declare (ignore request response))
  (error "boom from example route"))

(defun not-found-handler (request response)
  (declare (ignore request))
  (wst.routing:not-found-response t response :content "fallback route"))

(defun build-app-routes ()
  (wst.routing:condition-handler #'app-condition-handler)
  (let ((cb-before (getf *circuit-breaker* :before))
        (cb-after (getf *circuit-breaker* :after)))
    (wst.routing.dsl:build-webserver
     `(wst.routing.dsl:group
        (wst.routing.dsl:route :GET index "/" index-handler)
        (wst.routing.dsl:route :GET health "/health" health-handler)
        (wst.routing.dsl:route :GET boom "/boom" boom-handler)
        (wst.routing.dsl:resource "/api/v1"
                                  (wst.routing.dsl:route :GET users "/users" users-handler)
                                  (wst.routing.dsl:route :GET csrf "/csrf" csrf-token-handler)
                                  (wst.routing.dsl:wrap
                                   :before (,*parse-content-middleware* csrf-before)
                                   :route (wst.routing.dsl:route :POST echo "/echo" echo-handler))
                                  (wst.routing.dsl:route :GET cookies "/cookies" cookies-handler))
        (wst.routing.dsl:wrap
         :before (,cb-before rate-limit-before)
        :after (,cb-after)
        :route (wst.routing.dsl:route :GET flaky "/api/v1/flaky" flaky-handler))
       (wst.routing.dsl:any-route :GET not-found-handler)))))

(defun app (env)
  (let* ((request (wst.routing.woo:request-from-woo-env env))
         (response (wst.routing:dispatch-route request)))
    (wst.routing.woo:response-to-woo-response response)))

(defun start (&key (port 3000))
  (build-app-routes)
  (format t "~&Starting example app on http://localhost:~a~%" port)
  (woo:run #'app :port port))

(start)
