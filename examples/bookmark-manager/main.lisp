(defpackage #:wst.example.bookmark-manager
  (:use #:cl))

(in-package #:wst.example.bookmark-manager)

(defparameter *parse-content-middleware*
  (wst.request-content.routing:parse-request-content))
(defparameter *trust-forwarded-https-headers-p* nil)

(defun body-value (request key)
  (let ((body (wst.routing:request-content request)))
    (cond
      ((hash-table-p body) (or (gethash key body)
                               (gethash (intern (string-upcase key) :keyword) body)))
      ((listp body) (or (cdr (assoc key body :test #'string=))
                        (cdr (assoc (intern (string-upcase key) :keyword) body))))
      (t nil))))

(defun find-session-id-cookie (request)
  (let ((cookies (wst.cookies:parse-cookies (wst.routing:request-headers request))))
    (loop for cookie in cookies
          when (string= (wst.cookies:cookie-name cookie) "wst_session_id")
            do (return (wst.cookies:cookie-value cookie)))))

(defun current-user (request)
  (let ((session-id (find-session-id-cookie request)))
    (and session-id (session-user session-id))))

(defun request-header (request name)
  (let ((headers (wst.routing:request-headers request)))
    (or (gethash name headers)
        (gethash (string-downcase name) headers)
        (gethash (string-capitalize name) headers))))

(defun request-https-p (request)
  (and *trust-forwarded-https-headers-p*
       (or (string-equal (or (request-header request "X-Forwarded-Proto") "") "https")
           (string-equal (or (request-header request "X-Forwarded-Ssl") "") "on"))))

(defun session-cookie-value (request session-id)
  (format nil "wst_session_id=~a; Path=/; HttpOnly; SameSite=Lax; Max-Age=~a~:[~;; Secure~]"
          session-id
          *session-max-age-seconds*
          (request-https-p request)))

(defun clear-session-cookie-value (request)
  (format nil "wst_session_id=; Max-Age=0; Path=/; HttpOnly; SameSite=Lax~:[~;; Secure~]"
          (request-https-p request)))

(defun index-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response
   t response
   :content
   "wst + woo bookmark manager example
Demo authentication uses only a username (no password).
POST /api/v1/session/login with user=alice
POST /api/v1/bookmarks with title/url
GET /api/v1/bookmarks to list your bookmarks"))

(defun health-handler (request response)
  (declare (ignore request))
  (wst.routing:ok-response t response :content "ok"))

(defmethod wst.request-content:parse-content
    ((type (eql :|application/json|)) content &optional (encoding :utf-8))
  (declare (ignore type))
  (com.inuoe.jzon:parse (wst.request-content:content-as-string content encoding)))

(defun login-handler (request response)
  (let ((user (trim-whitespace (or (body-value request "user") ""))))
    (if (not (valid-username-p user))
        (wst.routing:write-response response
                                    :status 400
                                    :content "user must be 1-64 chars: letters, digits, '-', '_' or '.'")
        (let ((session-id (create-session user)))
          (wst.routing:write-response
           response
           :status 200
           :headers (list :set-cookie
                          (session-cookie-value request session-id))
           :content (format nil "logged in as ~a" user))))))

(defun logout-handler (request response)
  (let ((session-id (find-session-id-cookie request)))
    (when session-id
      (destroy-session session-id))
    (wst.routing:write-response
     response
     :status 200
     :headers (list :set-cookie (clear-session-cookie-value request))
     :content "logged out")))

(defun list-bookmarks-handler (request response)
  (let ((user (current-user request)))
    (if (null user)
        (wst.routing:unauthorized-response t response)
        (let ((bookmarks (bookmarks-for-user user)))
          (wst.routing:ok-response
           t response
           :content (if bookmarks
                        (with-output-to-string (out)
                          (dolist (row bookmarks)
                            (format out "~a. ~a -> ~a~%"
                                    (sqlite-row-column row 0)
                                    (sqlite-row-column row 1)
                                    (sqlite-row-column row 2))))
                        "no bookmarks yet"))))))

(defun create-bookmark-handler (request response)
  (let ((user (current-user request)))
    (if (null user)
        (wst.routing:unauthorized-response t response)
        (let* ((raw-url (body-value request "url"))
               (url (normalize-url raw-url))
               (title (trim-whitespace (or (body-value request "title") "")))
               (final-title (if (zerop (length title)) url title)))
          (if (or (null url)
                  (not (valid-http-url-p url)))
              (wst.routing:write-response response
                                          :status 400
                                          :content "url field is required and must be http/https")
              (progn
                (add-bookmark user final-title url)
                (wst.routing:write-response
                 response
                 :status 201
                 :content (format nil "saved bookmark ~a -> ~a" final-title url))))))))

(defun request-bookmark-id (request)
  (wst.routing:with-request-data (params) request
    (let ((raw (cdr (assoc "bookmark-id" params :test #'string-equal))))
      (parse-positive-integer raw))))

(defun delete-bookmark-handler (request response)
  (let ((user (current-user request)))
    (if (null user)
        (wst.routing:unauthorized-response t response)
        (let ((bookmark-id (request-bookmark-id request)))
          (if (or (null bookmark-id)
                  (not (bookmark-exists-for-user-p user bookmark-id)))
              (wst.routing:not-found-response t response :content "bookmark not found")
              (progn
                (delete-bookmark user bookmark-id)
                (wst.routing:ok-response t response :content (format nil "deleted bookmark ~a" bookmark-id))))))))

(defun not-found-handler (request response)
  (declare (ignore request))
  (wst.routing:not-found-response t response :content "route not found"))

(defun build-app-routes ()
  (wst.routing:condition-handler #'wst.routing:development-condition-handler)
  (wst.routing.dsl:build-webserver
   `(wst.routing.dsl:group
     (wst.routing.dsl:route :GET index "/" index-handler)
     (wst.routing.dsl:route :GET health "/health" health-handler)
     (wst.routing.dsl:resource
      "/api/v1"
      (wst.routing.dsl:wrap
       :before (,*parse-content-middleware*)
       :route (wst.routing.dsl:group
               (wst.routing.dsl:route :POST login "/session/login" login-handler)
               (wst.routing.dsl:route :POST logout "/session/logout" logout-handler)
               (wst.routing.dsl:route :POST create-bookmark "/bookmarks" create-bookmark-handler)))
      (wst.routing.dsl:route :GET list-bookmarks "/bookmarks" list-bookmarks-handler)
      (wst.routing.dsl:route :DELETE delete-bookmark "/bookmarks/:bookmark-id" delete-bookmark-handler))
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
  (sb-thread:make-mutex :name "bookmark-manager-server-control"))

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
  (with-database-lock
    (ensure-database-connection))
  (loop
    (build-app-routes)
    (format t "~&Starting bookmark manager on http://localhost:~a~%" *server-port*)
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
