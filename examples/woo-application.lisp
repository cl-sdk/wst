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

(defclass example-session-csrf-store ()
  ((tokens
    :initform (make-hash-table :test 'equal)
    :accessor store-tokens)))

(defparameter *csrf-store* (make-instance 'example-session-csrf-store))

(defun generate-random-token ()
  (labels ((bytes->hex (bytes)
             (with-output-to-string (out)
               (loop for b across bytes
                     do (format out "~2,'0X" b)))))
    (with-open-file (stream "/dev/urandom"
                            :direction :input
                            :element-type '(unsigned-byte 8))
      (let ((bytes (make-array 32 :element-type '(unsigned-byte 8))))
        (unless (= (read-sequence bytes stream) (length bytes))
          (error "failed to read enough random bytes for csrf token"))
        (bytes->hex bytes)))))

(defun generate-csrf-token ()
  (generate-random-token))

(defun generate-session-id ()
  (generate-random-token))

(defun secure-string= (a b)
  (if (and (stringp a) (stringp b))
      (let* ((len-a (length a))
             (len-b (length b))
             (max-len (max len-a len-b))
             (acc (logxor len-a len-b)))
        (dotimes (i max-len (zerop acc))
          (let ((char-a (if (< i len-a) (char-code (aref a i)) 0))
                (char-b (if (< i len-b) (char-code (aref b i)) 0)))
            (setf acc (logior acc (logxor char-a char-b))))))
      nil))

(defmethod wst.session.csrf:session-csrf-token ((obj example-session-csrf-store) &key session-id &allow-other-keys)
  (gethash session-id (store-tokens obj)))

(defmethod wst.session.csrf:add-session-csrf-token ((obj example-session-csrf-store) key &key session-id &allow-other-keys)
  (setf (gethash session-id (store-tokens obj)) key))

(defmethod wst.session.csrf:remove-session-csrf-token ((obj example-session-csrf-store) &key session-id &allow-other-keys)
  (remhash session-id (store-tokens obj)))

(defmethod wst.session.csrf:verify-session-csrf-token ((obj example-session-csrf-store) key &key session-id &allow-other-keys)
  (let ((stored (wst.session.csrf:session-csrf-token obj :session-id session-id)))
    (and stored key (secure-string= stored key))))

(defun request-session-id (request)
  (let* ((cookies (wst.cookies:parse-cookies (wst.routing:request-headers request)))
         (session-id (cdr (assoc "wst-example-session-id" cookies :test #'string=))))
    session-id))

(defun csrf-token-handler (request response)
  (let* ((session-id (or (request-session-id request) (generate-session-id)))
         (token (generate-csrf-token)))
    (wst.session.csrf:add-session-csrf-token *csrf-store* token :session-id session-id)
    (wst.routing:ok-response t response
                             :headers (list :set-cookie (format nil "wst-example-session-id=~a; Path=/; SameSite=Strict" session-id)
                                            :x-csrf-token token)
                             :content token)))

(defun csrf-before (request response)
  (let ((method (wst.routing:request-method request)))
    (if (member method '(:POST :PUT :PATCH :DELETE))
        (let* ((headers (wst.routing:request-headers request))
               (session-id (request-session-id request))
               (header-token (gethash "x-csrf-token" headers))
               (stored-token (wst.session.csrf:session-csrf-token *csrf-store* :session-id session-id)))
           (cond
             ((or (null header-token) (null stored-token))
              (cons :halt (wst.routing:forbidden-response t response :content "Missing CSRF token")))
             ((wst.session.csrf:verify-session-csrf-token *csrf-store* header-token :session-id session-id)
              (cons :continue response))
             (t
              (cons :halt (wst.routing:forbidden-response t response :content "Invalid CSRF token")))))
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

(defun csrf-check-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response t response :content "csrf token valid"))

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
  (wst.routing:condition-handler #'wst.routing:development-condition-handler)

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
                                  :before (wst.example.woo-application::csrf-before)
                                  :route (wst.routing.dsl:route :POST csrf-check "/csrf/check" csrf-check-handler))
                                 (wst.routing.dsl:wrap
                                  :before (,*parse-content-middleware*)
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

(defconstant +sigint+ 2)
(defconstant +sigquit+ 3)
(defconstant +sigterm+ 15)
(defparameter *server-port* 3000)
(defparameter *server-running-p* nil)
(defparameter *restart-requested-p* nil)
#+sbcl
(defparameter *server-control-lock*
  (sb-thread:make-mutex :name "woo-example-server-control"))

(defmacro with-server-control-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*server-control-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun woo-signal-symbol (name)
  (or (find-symbol name :woo.signal)
      (error "Woo internal symbol ~a not found in package WOO.SIGNAL" name)))

(defun make-graceful-shutdown-signals ()
  "Map SIGINT/SIGQUIT/SIGTERM to Woo's graceful shutdown callback."
  (let ((graceful-callback-symbol (woo-signal-symbol "SIGQUIT-CB")))
    (list (cons +sigint+ graceful-callback-symbol)
          (cons +sigquit+ graceful-callback-symbol)
          (cons +sigterm+ graceful-callback-symbol))))

(defun request-graceful-stop ()
  #+sbcl
  (sb-posix:kill (sb-posix:getpid) +sigquit+)
  #-sbcl
  (error "Restarting a running server is only supported on SBCL."))

(defun start (&key (port 3000))
  (with-server-control-lock
    (when *server-running-p*
      (error "The example app is already running. Use (restart) to restart it."))
    (setf *server-port* port
          *restart-requested-p* nil))
  (loop
    (build-app-routes)
    (format t "~&Starting example app on http://localhost:~a~%" *server-port*)
    (format t "~&Press Ctrl+C to stop gracefully.~%")
    (let ((signals-symbol (woo-signal-symbol "*SIGNALS*")))
      (unwind-protect
           (progn
             (with-server-control-lock
               (setf *server-running-p* t))
             (progv (list signals-symbol) (list (make-graceful-shutdown-signals))
               (woo:run #'app :port *server-port*)))
        (with-server-control-lock
          (setf *server-running-p* nil))))
    (unless (with-server-control-lock
              (prog1 *restart-requested-p*
                (setf *restart-requested-p* nil)))
      (return))
    (format t "~&Restarting example app...~%")))

(defun restart (&key (port *server-port*))
  (multiple-value-bind (running-p target-port)
      (with-server-control-lock
        (setf *server-port* port)
        (if *server-running-p*
            (progn
              (setf *restart-requested-p* t)
              (values t *server-port*))
            (values nil *server-port*)))
    (if running-p
        (request-graceful-stop)
        (start :port target-port))))

(start)
