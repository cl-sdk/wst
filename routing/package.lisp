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
   #:find-route-by-name
   #:forbidden-response))

(in-package :wst.routing)

(defstruct request
  "Request object."
  (uri "" :type string)
  (method :GET :type symbol)
  (headers (cl-hash-util:hash-create nil) :type hash-table)
  (content-type nil :type (or string null))
  (content-length 0 :type integer)
  content
  (data nil :type list))

(defstruct response
  "Response object."
  (status 0 :type integer)
  (headers nil :type list)
  (content "" :type string)
  (data nil :type list))

(defvar *routes* nil
  "Hash to hold all routes.")
