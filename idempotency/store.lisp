(defpackage #:io.github.cl-sdk.wst.idempotency.store
  (:use #:cl)
  (:documentation "Storage backend protocol for idempotency key tracking.

Implementations can provide atomic semantics for store-claim/store-complete/store-release in
single-node or distributed stores.")
  (:export
   #:idempotency-entry
   #:make-idempotency-entry
   #:idempotency-entry-state
   #:idempotency-entry-fingerprint
   #:idempotency-entry-response
   #:idempotency-entry-expires-at
   #:store-claim-idempotency
   #:store-complete-idempotency
   #:store-release-idempotency))

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

(defgeneric store-claim-idempotency (store key fingerprint ttl-seconds now)
  (:documentation "Try to claim KEY for FINGERPRINT.

Returns two values:
- STATUS keyword: one of :started, :replay, :in-progress, :conflict
- ENTRY (or NIL): an idempotency-entry when useful for caller decisions."))

(defgeneric store-complete-idempotency (store key fingerprint response ttl-seconds now)
  (:documentation "Persist RESPONSE as completed for KEY/FINGERPRINT.

Returns T when completion happened, NIL otherwise."))

(defgeneric store-release-idempotency (store key fingerprint)
  (:documentation "Release a processing entry for KEY/FINGERPRINT.

Used when the current request should not be cached (for example 5xx).
Returns T when release happened, NIL otherwise."))
