(defpackage #:io.github.cl-sdk.wst.idempotency
  (:use #:cl)
  (:documentation "Core idempotency-key primitives independent of HTTP objects.")
  (:import-from #:io.github.cl-sdk.wst.idempotency.store
                #:create-entry
                #:update-entry
                #:delete-entry
                #:idempotency-entry-response)
  (:export
   #:cached-response
   #:make-cached-response
   #:cached-response-status
   #:cached-response-headers
   #:cached-response-content
   #:idempotency-engine
   #:make-idempotency-engine
   #:idempotency-engine-store
   #:idempotency-engine-ttl-seconds
   #:idempotency-engine-clock
   #:idempotency-engine-cache-response-p
   #:valid-idempotency-key-p
   #:register-request
   #:drop-request
   #:store-response))

(in-package #:io.github.cl-sdk.wst.idempotency)

(defstruct cached-response
  "Serializable response snapshot for replay."
  status
  headers
  content)

(defclass idempotency-engine ()
  ((ttl-seconds :initarg :ttl-seconds
                :initform 86400
                :reader idempotency-engine-ttl-seconds)
   (clock :initarg :clock
          :initform #'get-universal-time
          :reader idempotency-engine-clock)
   (cache-response-p :initarg :cache-response-p
                     :initform (lambda (response)
                                 (< (cached-response-status response) 500))
                     :reader idempotency-engine-cache-response-p))
  (:documentation "Core idempotency orchestration object.

Engine subclasses can define their own storage slots and specialize
io.github.cl-sdk.wst.idempotency.store:create-entry,
io.github.cl-sdk.wst.idempotency.store:update-entry and
io.github.cl-sdk.wst.idempotency.store:delete-entry for storage behavior.
For a complete engine implementation, specialize all three methods."))

(defgeneric idempotency-engine-store (engine)
  (:documentation "Return the storage object owned by ENGINE.

Concrete engine implementations that delegate to a store object should
define this accessor."))

(defun make-idempotency-engine (&key
                                  (ttl-seconds 86400)
                                  (clock #'get-universal-time)
                                  (cache-response-p (lambda (response)
                                                      (< (cached-response-status response) 500))))
  (declare (ignore ttl-seconds clock cache-response-p))
  (error "No default store-backed idempotency engine is provided in io.github.cl-sdk.wst.idempotency. Use a concrete engine implementation such as io.github.cl-sdk.wst.idempotency.memory-store:make-memory-idempotency-engine or your own subclass."))

(defun valid-idempotency-key-p (value)
  "Return T when VALUE is a non-empty key of at most 255 chars.

Key formatting/normalization is caller-managed."
  (and (stringp value)
       (not (string= "" value))
       (<= (length value) 255)))

(defgeneric register-request (engine scope key fingerprint)
  (:documentation "Register lifecycle processing ownership for SCOPE/KEY/FINGERPRINT.

Returns two values:
- DECISION: one of :started, :replay, :in-progress, :conflict
- PAYLOAD: cached-response for :replay, NIL otherwise."))

(defmethod register-request ((engine idempotency-engine) scope key fingerprint)
  (multiple-value-bind (status entry)
      (create-entry engine
                    (list scope key)
                    fingerprint
                    (idempotency-engine-ttl-seconds engine)
                    (funcall (idempotency-engine-clock engine)))
    (ecase status
      (:started (values :started nil))
      (:in-progress (values :in-progress nil))
      (:conflict (values :conflict nil))
      (:replay (values :replay (idempotency-entry-response entry))))))

(defgeneric store-response (engine scope key fingerprint response)
  (:documentation "Persist RESPONSE for SCOPE/KEY/FINGERPRINT.

Returns T when completion happened, NIL otherwise."))

(defmethod store-response ((engine idempotency-engine) scope key fingerprint response)
  (update-entry engine
                (list scope key)
                fingerprint
                response
                (idempotency-engine-ttl-seconds engine)
                (funcall (idempotency-engine-clock engine))))

(defgeneric drop-request (engine scope key fingerprint)
  (:documentation "Release processing lock for SCOPE/KEY/FINGERPRINT.

Returns T when release happened, NIL otherwise."))

(defmethod drop-request ((engine idempotency-engine) scope key fingerprint)
  (delete-entry engine
                (list scope key)
                fingerprint))
