(ql:quickload '(:wst.routing
                :wst.routing.dsl
                :wst.routing.response.dsl
                :wst.routing.woo
                :wst.request-content
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
  (let* ((content-type (or (wst.routing:request-content-type request) "text/plain"))
         (parsed-type (or (car (wst.request-content:parse-content-type content-type))
                          (cons :|text/plain| nil))))
    (destructuring-bind (mime . options) parsed-type
      (let* ((charset (cdr (assoc "charset" options :test #'string=)))
              (encoding (if (string-equal charset "utf-8")
                            :utf-8
                            :us-ascii)))
        (handler-case
            (let ((body (wst.request-content:parse-content
                         mime
                         (wst.routing:request-content request)
                         encoding)))
              (wst.routing:ok-response t response :content (format nil "~a" body)))
          (error ()
            (wst.routing:bad-request-response t response))))
      )))

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
                                 (wst.routing.dsl:route :POST echo "/echo" echo-handler)
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
