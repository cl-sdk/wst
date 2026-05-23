(defpackage #:io.github.cl-sdk.wst.idempotency.store
  (:use #:cl)
  (:documentation "Storage backend protocol for idempotency key tracking.

Implementations can provide atomic semantics for create/update/delete entry
operations in single-node or distributed stores.")
  (:export
   #:idempotency-entry
   #:make-idempotency-entry
   #:idempotency-entry-state
   #:idempotency-entry-fingerprint
   #:idempotency-entry-response
   #:idempotency-entry-expires-at
   #:create-entry
   #:update-entry
   #:delete-entry))

(in-package #:io.github.cl-sdk.wst.idempotency.store)

(defstruct idempotency-entry
  "Stored lifecycle record for a key.

STATE is one of:
- :processing
- :completed"
  state
  fingerprint
  response
  expires-at)

(defgeneric create-entry (store key fingerprint ttl-seconds now)
  (:documentation "Try to claim KEY for FINGERPRINT.

Returns two values:
- STATUS keyword: one of :started, :replay, :in-progress, :conflict
- ENTRY (or NIL): an idempotency-entry when useful for caller decisions."))

(defgeneric update-entry (store key fingerprint response ttl-seconds now)
  (:documentation "Persist RESPONSE as completed for KEY/FINGERPRINT.

Returns T when completion happened, NIL otherwise."))

(defgeneric delete-entry (store key fingerprint)
  (:documentation "Release a processing entry for KEY/FINGERPRINT.

Used when the current request should not be cached (for example 5xx).
Returns T when release happened, NIL otherwise."))
