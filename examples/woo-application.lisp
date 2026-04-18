;;; Run with:
;;;   sbcl --load examples/woo-application.lisp
;;;
;;; Then test:
;;;   curl -i http://localhost:3000/
;;;   curl -i http://localhost:3000/health
;;;   curl -i http://localhost:3000/api/v1/users
;;;   curl -i -X POST http://localhost:3000/api/v1/echo \
;;;        -H "Content-Type: application/x-www-form-urlencoded; charset=utf-8" \
;;;        -d "name=wst&lang=lisp"
;;;   curl -i http://localhost:3000/api/v1/cookies -H "Cookie: first=one; second=two"
;;;   curl -i http://localhost:3000/api/v1/flaky?fail=true

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

(defun users-handler (request response)
  (declare (ignore request))
  (wst.routing.response.dsl:status 200 response)
  (wst.routing.response.dsl:json t "{\"users\":[{\"id\":1,\"name\":\"alice\"}]}" response))

(defun echo-handler (request response)
  (let* ((content-type (or (wst.routing:request-content-type request) "text/plain"))
         (parsed-type (car (wst.request-content:parse-content-type content-type)))
         (mime (or (car parsed-type) :|text/plain|))
         (options (cdr parsed-type))
         (charset (cdr (assoc "charset" options :test #'string=)))
         (encoding (or (and charset
                            (ignore-errors (intern (string-upcase charset) :keyword)))
                       :us-ascii))
         (body (wst.request-content:parse-content
                mime
                (wst.routing:request-content request)
                encoding)))
    (wst.routing:ok-response t response :content (format nil "~a" body))))

(defun cookies-handler (request response)
  (let* ((cookies (wst.cookies:parse-cookies (wst.routing:request-headers request))))
    (wst.routing:ok-response t response
                             :content (format nil "cookies=~a" (length cookies)))))

(defun flaky-handler (request response)
  (if (search "fail=true" (wst.routing:request-query request) :test #'char-equal)
      (wst.routing:internal-server-error-response t response :content "forced failure")
      (wst.routing:ok-response t response :content "stable response")))

(defun not-found-handler (request response)
  (declare (ignore request))
  (wst.routing:not-found-response t response :content "fallback route"))

(defun build-app-routes ()
  (wst.routing.dsl:build-webserver
   `(wst.routing.dsl:group
     (wst.routing.dsl:route :GET index "/" index-handler)
     (wst.routing.dsl:route :GET health "/health" health-handler)
     (wst.routing.dsl:resource "/api/v1"
                              (wst.routing.dsl:route :GET users "/users" users-handler)
                              (wst.routing.dsl:route :POST echo "/echo" echo-handler)
                              (wst.routing.dsl:route :GET cookies "/cookies" cookies-handler))
     (wst.routing.dsl:wrap
      :before (list ,(getf *circuit-breaker* :before) rate-limit-before)
      :after (list ,(getf *circuit-breaker* :after))
      :route (wst.routing.dsl:route :GET flaky "/api/v1/flaky" flaky-handler))
     (wst.routing.dsl:any-route :GET not-found-handler))))

(defun app (env)
  (let* ((request (wst.routing.woo:request-from-woo-env env))
         (response (wst.routing:dispatch-route request)))
    (wst.routing.woo:response-to-woo-response response)))

(defun start (&key (port 3000))
  (build-app-routes)
  (format t "~&Starting example app on http://localhost:~a~%" port)
  (woo:run #'app :port port))

(start)
