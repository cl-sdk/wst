(defpackage #:wst.routing
  (:use #:cl)
  (:import-from #:cl-hash-util
		#:hash-create
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
   #:request
   #:response
   #:dispatch-route
   #:dispatch-route-by-name
   #:route
   #:find-route-by-name
   #:remove-route
   #:add-route
   #:condition-handler
   #:any-route-handler
   #:ok-response
   #:internal-server-error-response
   #:not-found-response
   #:forbidden-response
   #:unauthorized-response
   #:bad-request-response
   #:redirect-see-other-response
   #:make-request
   #:make-response
   #:request-data
   #:request-uri
   #:request-method
   #:request-headers
   #:request-content
   #:request-content-type
   #:response-status
   #:response-headers
   #:response-content
   #:response-data
   #:request-content-length
   #:write-response
   #:unprocessable-entity))

(in-package :wst.routing)

(defstruct request
  "Request object."
  (uri "" :type string)
  (method :GET :type symbol)
  (headers (hash-create nil) :type hash-table)
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
