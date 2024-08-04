(defpackage #:wst.routing
  (:use #:cl)
  (:import-from #:cl-hash-util
		#:hash
		#:with-keys)
  (:import-from #:alexandria
		#:ensure-list)
  (:import-from #:str
		#:split
		#:join)
  (:import-from #:flexi-streams
		#:make-flexi-stream)
  (:import-from #:com.inuoe.jzon
		#:parse)
  (:import-from #:uiop
		#:read-file-string)
  (:export
   #:any-route-handler
   #:route
   #:matcher
   #:request-json-content
   #:request-content-stream
   #:add-route
   #:remove-route
   #:dispatch-route
   #:dispatch-route-by-name
   #:response
   #:not-found-response
   #:internal-server-error-response
   #:condition-handler
   #:change-static-path
   #:route-static
   #:request-method
   #:request-headers
   #:request-content
   #:request-data
   #:response-status
   #:response-headers
   #:response-content
   #:response-data
   #:make-request
   #:make-response
   #:success-response
   #:unauthorized
   #:write-response
   #:bad-request
   #:request
   #:redirect-see-other
   #:request-content-type
   #:request-content-length
   #:woo-env->request))

(in-package :wst.routing)

(defstruct request
  method
  params
  headers
  content-type
  content-length
  content
  data)

(defstruct response
  status
  headers
  content
  data)

(defun write-response (response
		       &key
			 content
			 (status 200)
			 headers
			 (content-type "text/html" content-type-boundp))
  (setf (response-status response) status
	(response-headers response) (append (response-headers response)
					    (list :content-type content-type)
					    headers)
	(response-content response) content)
  response)

(defgeneric success-response (ty response &key headers content)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) response &key headers content)
    (write-response response :status 200 :headers headers :content content)))

(Defun default-internal-server-error-resounse (response)
  (write-response response :status 500 :content "internal server error"))

(defgeneric internal-server-error-response (ty response &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) response &key)
    (default-internal-server-error-resounse response)))

(defgeneric not-found-response (ty response &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) response &key)
    (write-response response :status 404 :content "not found")))

(defgeneric unauthorized (ty response &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) response &key)
    (write-response response :status 401 :content "unauthorized")))

(defgeneric bad-request (ty response &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) response &key)
    (write-response response :status 400 :content "bad request")))

(defgeneric redirect-see-other (ty response location &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) response location &key)
    (write-response response :status 303 :content "see-other"
			     :headers (list :location location))))

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

(defvar +text/html+ "text/html")
(defvar +application/json+ "application/json")

(defvar +empty-content+
  (cons "" nil)
  "Used when there is no content")

(defvar *route-specs* nil
  "Hash to hold all routes.")

(defparameter *static-path* *default-pathname-defaults*
  "Path to find all static files.")

(defparameter *condition-handler* nil
  "A user-defined function to handle conditions before
 calling the default internal server error.")

(defun condition-handler (fn)
  "Define FN as the function to handle conditions before
 calling the default internal server error."
  (setf *condition-handler* fn))

(defparameter *any-route-handler* nil
  "A user-defined function to handle conditions before
 calling the default internal server error.")

(defun any-route-handler (method fn)
  "Define FN as the function to handle conditions before
 calling the default internal server error."
  (setf *any-route-handler*
	(make-route :name 'any-route
		    :method method
		    :path nil
		    :matcher nil
		    :dispatcher fn)))

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

(defun bool->integer (x)
  "Convert boolean into an integer."
  (if x 1 0))

(declaim (inline get-item-from-from-request))
(defun get-item-from-from-request (item request)
  "Find a HTTP header ITEM on the REQUEST."
  (flet ((check-value (value)
	   (if value
	       (cadr value)
	       (error (format nil "unable to get `~a` content." item)))))
    (serapeum:~>>
     request
     (member item)
     (check-value))))

(declaim (inline request-json-content))
(defun request-json-content (request)
  "Returns the REQUEST content as a json object."
  (parse (request-content-stream request)))

(defun change-static-path (path)
  "Change static files path to PATH."
  (setf *static-path* path))

(defun build-matcher (path method)
  "Build the matcher for PATH and METHOD."
  (let ((segments
	  (remove-if (lambda (p) (or (null p) (= 0 (length p))))
		     (cdr (split "/" path)))))
    (make-matcher :method method
		  :segments-count (length segments)
		  :segments segments)))

(defun add-route (name path method dispatcher)
  "Add a new route associating a NAME, PATH and METHOD to a DISPATCHER."
  (let ((route (make-route :name name
			   :path path
			   :method method
			   :matcher (build-matcher path method)
			   :dispatcher dispatcher)))
    (setf *route-specs* (append *route-specs* (list route)))))

(defun remove-route (name)
  "Remove a route associate by NAME."
  (setf *route-specs*
	(remove-if (lambda (route)
		     (equal name (route-name route)))
		   *route-specs*)))

(defun match (matcher method segments count)
  "Run the MATCHER for METHOD, SEGMENTS and COUNT."
  (if (or (not (= count (matcher-segments-count matcher)))
	 (not (equal method (matcher-method matcher))))
      :skip
      (remove-if #'null
		 (loop :for x :in (matcher-segments matcher)
		       :for y :in segments
		       collect (let ((param? (str:starts-with? ":" x)))
				 (case (+ (bool->integer (equal x y))
					  (* 2 (bool->integer param?)))
				   (0 (return-from match :skip))
				   (1 nil)
				   (2 (cons (str:substring 1 (length x) x) y))))))))

(defun do-matcher (r method segments count)
  (let ((params (match (route-matcher r) method segments count)))
    (when (not (equal params :skip))
      (cons r params))))

(defun match-route (path method &optional (routes *route-specs*))
  "Find a route by PATH and METHOD."
  (let* ((segments
	   (remove-if (lambda (p) (or (null p) (= 0 (length p))))
		      (cdr (str:split "/" path))))
	 (count (length segments)))
    (loop :for r :in routes
	  :do (let ((params (do-matcher r method segments count)))
		(when (not (equal params :skip))
		  (return (cons r params)))))))

(defun %dispatcher (route request response)
  "The dispatcher for any kind of dispatch. ROUTE-DATA is a pair of a route and the params and a request object."
  (handler-case
      (let* ((fn (route-dispatcher (or route
				      *not-fount-route*)))
	     (rs (funcall fn request response)))
	rs)
    (t (err)
      (log:error "unhandled error ~a" err)
      (if *condition-handler*
	  (let ((rs (funcall *condition-handler* request response err)))
	    (if (not rs)
		(funcall #'default-internal-server-error-resounse response)
		rs))
	  (funcall #'default-internal-server-error-resounse response)))))

(defun parse-woo-request-cookies (cookies)
  (reduce (lambda (cookies pair)
	    (destructuring-bind (key value)
		(str:split "=" pair)
	      (setf (gethash key cookies) value)
	      cookies))
	  (cl-ppcre:split ";\\s?" cookies)
	  :initial-value (make-hash-table)))

(defun parse-woo-cookies (headers request response)
  (let* ((cookies-string (gethash "cookie" headers
				     (gethash "Cookie" headers "")))
	 (cookies (parse-woo-request-cookies cookies-string)))
    (setf (request-data request)
	  (append (request-data request)
		  (list :cookies cookies))
	  (response-data response)
	  (append (getf (response-data response) :cookies)
		  (list :cookies nil)))))

(defun woo-env->request (woo-request)
  (let ((headers (getf woo-request :headers)))
    (make-request :content (getf woo-request :raw-body)
		  :headers headers
		  :content-type (getf woo-request :content-type "text/html")
		  :content-length (getf woo-request :content-length 0)
		  :data (list :env woo-request)
		  :method (getf woo-request :request-method))))

(defun dispatch-route (path method woo-request)
  "Dispatch a route by its PATH and METHOD. Pass REQUEST to it."
  (let ((headers (getf woo-request :headers))
	(rq (woo-env->request woo-request))
	(rs (make-response :status 0
			   :headers nil
			   :content nil
			   :data nil))
	(found (or (match-route path method)
		  (match-route path method (list *any-route-handler*)))))
    (parse-woo-cookies headers rq rs)
    (if (not found)
	(%dispatcher nil rq rs)
	(destructuring-bind (route . params)
	    found
	  (progn
	    (setf (request-data rq)
		  (append (request-data rq) (list :params params)))
	    (%dispatcher route rq rs))))))

(defun dispatch-route-by-name (name woo-request &optional params)
  "Dispatch a route by its PATH and METHOD. Pass REQUEST to it."
  (let* ((headers (getf woo-request :headers))
	 (method (getf woo-request :request-method))
	 (rq (make-request :content (getf woo-request :raw-body)
			   :headers headers
			   :content-type (getf woo-request :content-type "text/html")
			   :content-length (getf woo-request :content-length 0)
			   :data (list :env woo-request :params params)
			   :method method))
	 (rs (make-response :status 0
			    :headers nil
			    :content nil
			    :data nil))
	 (r (or (find-if (lambda (route) (equal name (route-name route))) *route-specs*)
	       (and (eql method (route-method *any-route-handler*)) *any-route-handler*))))
    (log:info *route-specs* r)
    (parse-woo-cookies headers rq rs)
    (%dispatcher r rq rs)))

(defun assert-request-method (method request)
  "Check if METHOD is in EVN."
  (when (not (equal method (request-method request)))
    (error (format nil "request method doesn't match ~a, expected ~a" (request-method request) method))))

(defmacro route (name method path args &body body)
  "Define a route with NAME for its function name, PATH to be requested and
 ARGS and BODY for the function."
  `(progn
     (remove-route ',name)
     (defun ,name ,args
       (assert-request-method ,method ,(first args))
       ,@body)
     (add-route ',name ,path ,method #',name)))

(defmacro route-static (name path mime)
  "Define a route for a static file with NAME for the function's name,
 PATH to be requested and MIME type."
  `(progn
     (remove-route ',name)
     (defun ,name (request response)
       (declare (ignorable request response))
       (let* ((serving (concatenate 'string (namestring ,*static-path*) ,path))
	      (content (read-file-string serving)))
	 (log:info serving content)
	 (write-response response
			 :status 200
			 :content-type ,mime
			 :content content)))
     (add-route ',name ,path :get #',name)))
