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
   #:internal-server-error-response))

(in-package #:wst.routing)

(defgeneric response (ty content &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) content &key)
    content))

(defgeneric internal-server-error-response (ty content &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) content &key)
    "internal server error"))

(defgeneric not-found-response (ty content &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) content &key)
    "not found"))

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

(defun any-route-handler (fn)
  "Define FN as the function to handle conditions before
 calling the default internal server error."
  (setf *any-route-handler*
        (make-route :name 'any-route
                    :method nil
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

(declaim (inline request-content-stream))
(defun request-content-stream (request)
  "Returns the REQUEST content as stream."
  (make-flexi-stream
   (get-item-from-from-request :raw-body request)
   :external-format :utf-8))

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
    (setf *route-specs* (push route *route-specs*))))

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

(defun match-route (path method)
  "Find a route by PATH and METHOD."
  (let* ((segments
           (remove-if (lambda (p) (or (null p)
                                (= 0 (length p))))
                      (cdr (str:split "/" path))))
         (count (length segments)))
    (loop :for r :in *route-specs*
          do (let ((params (match (route-matcher r) method segments count)))
               (when (not (equal params :skip))
                 (return-from match-route (cons r params)))))))

(defun %dispatcher (route request params)
  "The dispatcher for any kind of dispatch. ROUTE-DATA is a pair of a route and the params and a request object."
  (handler-case
      (let* ((fn (route-dispatcher (or route
                                      *any-route-handler*
                                      *not-fount-route*)))
             (response (funcall fn request params)))
        (if (not response)
            (funcall *not-fount-route* request nil)
            response))
    (t (err)
      (log:error "unhandled error ~a" err)
      (if *condition-handler*
          (funcall *condition-handler* err)
          (funcall #'internal-server-error-response request nil)))))

(defun dispatch-route (path method request)
  "Dispatch a route by its PATH and METHOD. Pass REQUEST to it."
  (let ((found (match-route path method)))
    (if (not found)
        (%dispatcher nil request nil)
        (destructuring-bind (route . params)
            found
          (%dispatcher route request params)))))

(defun dispatch-route-by-name (name request &optional params)
  "Dispatch a route by its PATH and METHOD. Pass REQUEST to it."
  (%dispatcher (find-if (lambda (route) (equal name (route-name route))) *route-specs*)
               request
               params))

(defun assert-request-method (method env)
  "Check if METHOD is in EVN."
  (let ((request-method (get-item-from-from-request :request-method env)))
    (if (equal method request-method)
        env
        (error (format nil "request method doesn't match ~a, expected ~a" request-method method)))))

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
     (defun ,name (request params)
       (declare (ignorable request params))
       (list 200 '(:content-type ,mime)
             (list (read-file-string
                    (concatenate 'string ,*static-path* ,path)))))
     (add-route ',name ,path :get #',name)))
