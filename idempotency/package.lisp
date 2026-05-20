(defpackage #:io.github.cl-sdk.wst.idempotency
  (:use #:cl)
  (:documentation "Core idempotency-key primitives independent of HTTP objects.")
  (:import-from #:io.github.cl-sdk.wst.idempotency.store
                #:claim-idempotency
                #:complete-idempotency
                #:release-idempotency
                #:idempotency-entry-response)
  (:import-from #:io.github.cl-sdk.wst.idempotency.memory-store
                #:memory-store)
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
   #:make-fingerprint
   #:begin-idempotency
   #:finish-idempotency))

(in-package #:io.github.cl-sdk.wst.idempotency)

(defstruct cached-response
  "Serializable response snapshot for replay."
  status
  headers
  content)

(defstruct (idempotency-engine
            (:constructor make-idempotency-engine
                          (&key
                             (store (make-instance 'memory-store))
                             (ttl-seconds 86400)
                             (clock #'get-universal-time)
                             (cache-response-p (lambda (response)
                                                 (< (cached-response-status response) 500))))))
  "Core idempotency orchestration object."
  store
  ttl-seconds
  clock
  cache-response-p)


(defun make-fingerprint (&key method scope body)
  "Build a deterministic fingerprint string."
  (with-output-to-string (stream)
    (prin1 (list :method method :scope scope :body body) stream)))

(defun begin-idempotency (engine scope key fingerprint)
  "Begin lifecycle for (SCOPE, KEY, FINGERPRINT).

Returns two values:
- DECISION: one of :started, :replay, :in-progress, :conflict
- PAYLOAD:  cached-response for :replay, NIL otherwise."
  (check-type engine idempotency-engine)
  (check-type scope t)
  (check-type key string)
  (check-type fingerprint string)
  (multiple-value-bind (status entry)
      (claim-idempotency (idempotency-engine-store engine)
                         (list scope key)
                         fingerprint
                         (idempotency-engine-ttl-seconds engine)
                         (funcall (idempotency-engine-clock engine)))
    (ecase status
      (:started (values :started nil))
      (:in-progress (values :in-progress nil))
      (:conflict (values :conflict nil))
      (:replay (values :replay (idempotency-entry-response entry))))))

(defun finish-idempotency (engine scope key fingerprint response)
  "Finalize lifecycle for a started key.

When response passes CACHE-RESPONSE-P policy, it is persisted for replay.
Otherwise the processing marker is released."
  (check-type engine idempotency-engine)
  (check-type key string)
  (check-type fingerprint string)
  (check-type response cached-response)
  (let* ((store (idempotency-engine-store engine))
         (scoped-key (list scope key))
         (now (funcall (idempotency-engine-clock engine))))
    (if (funcall (idempotency-engine-cache-response-p engine) response)
        (complete-idempotency store
                              scoped-key
                              fingerprint
                              response
                              (idempotency-engine-ttl-seconds engine)
                              now)
        (release-idempotency store scoped-key fingerprint))))
