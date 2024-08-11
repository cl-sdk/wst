(in-package :wst.routing)

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

(defun default-internal-server-error-resounse (response)
  (write-response response :status 500 :content "internal server error"))

(defgeneric internal-server-error-response (ty response &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) response &key)
    (default-internal-server-error-resounse response)))

(defgeneric not-found-response (ty response &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) response &key content)
    (write-response response :status 404 :content (or content "not found"))))

(defgeneric forbidden-response (ty response &key)
  (:documentation "Build a response for a type TY (:json, :html, t = html).
 CONTENT is any object that is serialized accourding to the type.")
  (:method ((ty t) response &key content)
    (write-response response :status 403 :content (or content "Forbidden"))))

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
