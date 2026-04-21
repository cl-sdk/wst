(defpackage #:wst.example.url-shortener
  (:use #:cl))

(in-package #:wst.example.url-shortener)

(defparameter *parse-content-middleware*
  (wst.request-content.routing:parse-request-content))

(defparameter *base-url* "http://localhost:3000")
(defparameter *short-url-store* (make-hash-table :test 'equal))
(defparameter *next-short-id* 0)
(defparameter +base62-alphabet+ "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defconstant +http-scheme-prefix+ "http://")
(defconstant +https-scheme-prefix+ "https://")
#+sbcl
(defparameter *short-url-store-lock*
  (sb-thread:make-mutex :name "woo-url-shortener-store"))

(defmacro with-short-url-store-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*short-url-store-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun integer->base62 (value)
  (if (zerop value)
      "0"
      (loop with result = ""
            with quotient = value
            while (> quotient 0)
            for remainder = (mod quotient 62)
            do (setf result (concatenate 'string
                                         (string (char +base62-alphabet+ remainder))
                                         result)
                     quotient (floor quotient 62))
            finally (return result))))

(defun create-short-url (target-url)
  (with-short-url-store-lock
    (let ((candidate (integer->base62 (incf *next-short-id*))))
      (setf (gethash candidate *short-url-store*) target-url)
      candidate)))

(defun find-short-url (code)
  (with-short-url-store-lock
    (gethash code *short-url-store*)))

(defun remove-short-url (code)
  (with-short-url-store-lock
    (remhash code *short-url-store*)))

(defun all-short-urls ()
  (with-short-url-store-lock
    (loop for code being the hash-keys of *short-url-store*
          using (hash-value target-url)
          collect (cons code target-url))))

(defun trim-whitespace (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return) text))

(defun starts-with-http-scheme-p (url)
  (or (and (>= (length url) (length +http-scheme-prefix+))
           (string-equal +http-scheme-prefix+ url :end2 (length +http-scheme-prefix+)))
      (and (>= (length url) (length +https-scheme-prefix+))
           (string-equal +https-scheme-prefix+ url :end2 (length +https-scheme-prefix+)))))

(defun normalize-target-url (url)
  (when (stringp url)
    (let ((trimmed (trim-whitespace url)))
      (unless (zerop (length trimmed))
        (if (starts-with-http-scheme-p trimmed)
            trimmed
            (format nil "https://~a" trimmed))))))

(defun request-url (request)
  (let ((body (wst.routing:request-content request)))
    (cond
      ((hash-table-p body)
       (or (gethash "url" body)
           (gethash :url body)))
      ((listp body)
       (or (cdr (assoc "url" body :test #'string=))
           (cdr (assoc :url body))))
      (t nil))))

(defun index-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response
   t response
   :content
   "wst + woo url shortener example\nPOST /api/v1/links with url=... then open GET /:code"))

(defun health-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response t response :content "ok"))

(defmethod wst.request-content:parse-content
    ((type (eql :|application/json|)) content &optional (encoding :utf-8))
  (declare (ignore type))
  (com.inuoe.jzon:parse (wst.request-content:content-as-string content encoding)))

(defun create-short-url-handler (request response)
  (let* ((raw-url (request-url request))
         (target-url (normalize-target-url raw-url)))
    (if (null target-url)
        (wst.routing:write-response response :status 400 :content "url field is required and cannot be empty")
        (let* ((code (create-short-url target-url))
               (short-path (format nil "/~a" code))
               (short-url (format nil "~a~a" *base-url* short-path)))
          (wst.routing:write-response
           response
           :status 201
           :headers (list :location short-path)
           :content (format nil "short_url=~a~%code=~a~%target=~a"
                            short-url code target-url))))))

(defun request-short-code (request)
  (wst.routing:with-request-data (params) request
    (cdr (assoc "code" params :test #'string-equal))))

(defun list-short-urls-handler (request response)
  (declare (ignore request))
  (let ((links (sort (all-short-urls) #'string< :key #'car)))
    (wst.routing:ok-response
     t response
     :content (if links
                  (with-output-to-string (out)
                    (dolist (link links)
                      (format out "~a -> ~a~%" (car link) (cdr link))))
                  "no short urls created yet"))))

(defun inspect-short-url-handler (request response)
  (let* ((code (request-short-code request))
         (target-url (and code (find-short-url code))))
    (if target-url
        (wst.routing:ok-response t response :content (format nil "~a -> ~a" code target-url))
        (wst.routing:not-found-response t response :content "short code not found"))))

(defun delete-short-url-handler (request response)
  (let* ((code (request-short-code request))
         (target-url (and code (find-short-url code))))
    (if target-url
        (progn
          (remove-short-url code)
          (wst.routing:ok-response t response :content (format nil "deleted /~a" code)))
        (wst.routing:not-found-response t response :content "short code not found"))))

(defun redirect-short-url-handler (request response)
  (let* ((code (request-short-code request))
         (target-url (and code (find-short-url code))))
    (if target-url
        (wst.routing:redirect-see-other-response t response target-url)
        (wst.routing:not-found-response t response :content "short code not found"))))

(defun not-found-handler (request response)
  (declare (ignore request))
  (wst.routing:not-found-response t response :content "route not found"))

(defun build-app-routes ()
  (wst.routing:condition-handler #'wst.routing:development-condition-handler)
  (wst.routing.dsl:build-webserver
   `(wst.routing.dsl:group
     (wst.routing.dsl:route :GET index "/" index-handler)
     (wst.routing.dsl:route :GET health "/health" health-handler)
     (wst.routing.dsl:resource "/api/v1"
                               (wst.routing.dsl:wrap
                                :before (,*parse-content-middleware*)
                                :route (wst.routing.dsl:route :POST create-short-url "/links" create-short-url-handler))
                               (wst.routing.dsl:route :GET list-short-urls "/links" list-short-urls-handler)
                               (wst.routing.dsl:route :GET inspect-short-url "/links/:code" inspect-short-url-handler)
                               (wst.routing.dsl:route :DELETE delete-short-url "/links/:code" delete-short-url-handler))
     (wst.routing.dsl:route :GET redirect-short-url "/:code" redirect-short-url-handler)
     (wst.routing.dsl:any-route :GET not-found-handler))))

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

(defun start-server (&key (port 3000))
  (with-server-control-lock
    (when *server-running-p*
      (error "The example app is already running. Use (restart-server) to restart it."))
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

(defun restart-server (&key (port *server-port*))
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
        (start-server :port target-port))))

(start-server)
