(defpackage #:wst.routing.woo
  (:use #:cl)
  (:import-from #:wst.routing
		#:response-content
		#:response-headers
		#:response-status
		#:make-request)
  (:export
   #:woo-env->request
   #:response->woo-response))

(in-package :wst.routing.woo)

(defun woo-env->request (env)
  (make-request :uri (getf env :request-uri)
		:headers (getf env :headers)
		:method (getf env :request-method)
		:content-type (getf env :content-type)
		:content-length (or (getf env :content-length)
				   0)
		:content (getf env :raw-body)
		:data (list :env env)))

(defun response->woo-response (response)
  (list (response-status response)
	(response-headers response)
	(list (response-content response))))
