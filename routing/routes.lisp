(in-package :wst.routing)

(defstruct route
  name
  method
  path
  matcher
  dispatcher)

(defstruct matcher
  method
  segments-count
  segments)

(defvar *not-fount-route*
  (make-route :name 'not-found
              :path nil
              :method nil
              :matcher nil
              :dispatcher #'not-found-response)
  "Route to be executed for not found.")

(defvar *internal-error-route*
  (make-route :name 'internal-error
              :path nil
              :method nil
              :matcher nil
              :dispatcher #'internal-server-error-response)
  "Route to be executed for internal server error.")

(defparameter *condition-handler* nil
  "A user-defined function to handle conditions before
 calling the default internal server error.")

(defparameter *any-route-handler* nil
  "A user-defined function to handle all routes.")

(defun condition-handler (fn)
  "Define FN as the function to handle conditions before
 calling the default internal server error."
  (setf *condition-handler* fn))

(defun any-route-handler (method fn)
  "Define FN as the function to handle conditions before
 calling the default internal server error."
  (setf *any-route-handler*
        (make-route :name 'any-route
                    :method method
                    :path nil
                    :matcher nil
                    :dispatcher fn)))

(declaim (ftype (function (string symbol) matcher)
                build-matcher))
(defun build-matcher (path method)
  "Build the matcher for PATH and METHOD."
  (let ((segments (remove-if (lambda (p) (or (null p) (= 0 (length p))))
                             (cdr (split "/" path)))))
    (make-matcher :method method
                  :segments-count (length segments)
                  :segments segments)))

(declaim (ftype (function (symbol string symbol function) t)
                add-route))
(defun add-route (name path method dispatcher)
  "Add a new route associating a NAME, PATH and METHOD to a DISPATCHER."
  (let ((route (make-route :name name
                           :path path
                           :method method
                           :matcher (build-matcher path method)
                           :dispatcher dispatcher)))
    (setf *routes* (append *routes* (list route)))
    t))

(defun parse-uri (uri)
  (let* ((hash-position (or (position #\# uri) (length uri)))
         (query-position (or (position #\? uri) hash-position)))
    (let ((path (str:substring 0 query-position uri))
          (query (str:substring (1+ query-position) hash-position uri))
          (hash (str:substring (1+ hash-position) (length uri) uri)))
      (values path query hash))))

(declaim (ftype (function (symbol) t)
                remove-route))
(defun remove-route (name)
  "Remove a route associate by NAME."
  (setf *routes*
        (remove-if (lambda (route)
                     (equal name (route-name route)))
                   *routes*))
  t)

(declaim (ftype (function (matcher symbol list integer) list)
                match))
(defun match (matcher method segments count)
  "Run the MATCHER for METHOD, SEGMENTS and COUNT."
  (if (or (not (= count (matcher-segments-count matcher)))
          (not (equal method (matcher-method matcher))))
      (list :skip nil)
      (list :params (loop :for x :in (matcher-segments matcher)
                          :for y :in segments
                          :if (str:starts-with? ":" x)
                            :collect (cons (str:substring 1 (length x) x) y)
                          :else :if (not (equal x y))
                                  :do (return-from match (list :skip nil))))))

(declaim (ftype (function (route symbol list integer) list)
                do-matcher))
(defun do-matcher (route method segments count)
  (destructuring-bind (action params)
      (match (route-matcher route) method segments count)
    (when (equal action :params)
      (cons route params))))

(declaim (ftype (function (string symbol &optional list) list)
                match-route))
(defun match-route (path method &optional (routes *routes*))
  "Find a route by PATH and METHOD."
  (let* ((segments
           (remove-if (lambda (p) (or (null p) (= 0 (length p))))
                      (cdr (str:split "/" path))))
         (count (length segments)))
    (loop :for route :in routes
          :do (alexandria:when-let ((match-data (do-matcher route method segments count)))
                (return match-data)))))

(declaim (ftype (function (string) hash-table)
                parse-cookies-string))
(defun parse-cookies-string (cookies)
  (reduce (lambda (cookies pair)
            (destructuring-bind (key value)
                (str:split "=" pair)
              (setf (gethash key cookies) value)
              cookies))
          (cl-ppcre:split ";\\s?" cookies)
          :initial-value (make-hash-table)))

(declaim (ftype (function (hash-table request response) t)
                parse-cookies))
(defun parse-cookies (headers request response)
  (let* ((cookies-string (gethash "cookie" headers (gethash "Cookie" headers "")))
         (cookies (parse-cookies-string cookies-string)))
    (setf (request-data request)
          (append (request-data request)
                  (list :cookies cookies))
          (response-data response)
          (append (getf (response-data response) :cookies)
                  (list :cookies nil)))
    t))

(declaim (ftype (function (symbol) (or route null))
                find-route-by-name))
(defun find-route-by-name (name)
  "Find a route by NAME."
  (find name *routes* :test 'equal :key 'route-name))

(defun %dispatcher (route request response)
  "The dispatcher for any kind of dispatch. ROUTE-DATA is a pair of a route and the params and a request object."
  (handler-case
      (let* ((fn (route-dispatcher (or route
                                      *not-fount-route*)))
             (rs (funcall fn request response)))
        rs)
    (t (err)
      (log:error "unhandled error ~a" err)
      (or (and *condition-handler* (funcall *condition-handler* request response err))
         (funcall #'default-internal-server-error-resounse response)))))

(defun dispatch-route (request)
  "Dispatch a route by its PATH and METHOD. Pass REQUEST to it."
  (with-slots (method headers uri)
      request
    (let* ((response (make-response))
           (found (or (match-route uri method)
                      (and *any-route-handler*
                           (equal (request-method request) (route-method *any-route-handler*))
                           (cons *any-route-handler* nil)))))
      (parse-cookies headers request response)
      (if (not found)
          (%dispatcher nil request response)
          (destructuring-bind (route . params)
              found
            (progn
              (setf (request-data request)
                    (append (request-data request) (list :params params)))
              (%dispatcher route request response)))))))

(defun dispatch-route-by-name (name request &optional params)
  "Dispatch a route by its PATH and METHOD. Pass REQUEST to it."
  (with-slots (method headers)
      request
    (let* ((response (make-response))
           (route (or (find-if (lambda (route) (equal name (route-name route))) *routes*)
                      (and *any-route-handler*
                           (eql method (route-method *any-route-handler*))
                           *any-route-handler*))))
      (log:info *routes* route)
      (parse-cookies headers request response)
      (setf (request-data request) (append (request-data request) (list :params params)))
      (%dispatcher route request response))))

(defmacro route (name method path args &body body)
  "Define a route with NAME for its function name, PATH to be requested and
 ARGS and BODY for the function."
  `(progn
     (remove-route ',name)
     (defun ,name ,args
       ,@body)
     (add-route ',name ,path ,method #',name)))
