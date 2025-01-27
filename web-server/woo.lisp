(defpackage #:wst.routing.woo
  (:use #:cl)
  (:export
   #:request-from-woo-env
   #:response-to-woo-response))

(in-package :wst.routing.woo)

(defun request-from-woo-env (env)
  (multiple-value-bind (path query hash)
      (wst.routing:parse-uri (getf env :request-uri))
    (wst.routing:make-request :uri path
                              :query query
                              :hash hash
                              :headers (getf env :headers)
                              :method (getf env :request-method)
                              :content-type (getf env :content-type)
                              :content-length (or (getf env :content-length) 0)
                              :content (getf env :raw-body)
                              :data (list :env env))))

(defun response-to-woo-response (response)
  (list (wst.routing:response-status response)
        (wst.routing:response-headers response)
        (list (wst.routing:response-content response))))
