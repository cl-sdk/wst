(defpackage #:io.github.cl-sdk.wst.idempotency.memory-store
  (:use #:cl #:io.github.cl-sdk.wst.idempotency.store)
  (:import-from #:io.github.cl-sdk.wst.idempotency
                #:idempotency-engine)
  (:documentation "In-memory idempotency backend.

Suitable for single-process scenarios; not thread-safe.")
  (:export
   #:memory-idempotency-engine))

(in-package #:io.github.cl-sdk.wst.idempotency.memory-store)

(defclass memory-idempotency-engine (idempotency-engine)
  ((table :initarg :table
          :initform (make-hash-table :test #'equal)
          :reader memory-idempotency-engine-table))
  (:documentation "Hash-table-backed in-memory idempotency engine.
Not thread-safe.

Instantiate with MAKE-INSTANCE and optional initargs:
- :table hash table used to persist entries (defaults to (make-hash-table :test #'equal))
- :ttl-seconds, :clock, :cache-response-p (inherited from idempotency-engine)."))

(defun %expired-p (entry now)
  (and entry
       (idempotency-entry-expires-at entry)
       (>= now (idempotency-entry-expires-at entry))))

(defmethod create-entry ((engine memory-idempotency-engine) key fingerprint ttl-seconds now)
  (let* ((table (memory-idempotency-engine-table engine))
         (entry (gethash key table)))
    (when (%expired-p entry now)
      (remhash key table)
      (setf entry nil))
    (cond
      ((null entry)
       (let ((new-entry (make-idempotency-entry
                         :state :processing
                         :fingerprint fingerprint
                         :response nil
                         :expires-at (+ now ttl-seconds))))
         (setf (gethash key table) new-entry)
         (values :started new-entry)))
      ((not (string= fingerprint (idempotency-entry-fingerprint entry)))
       (values :conflict entry))
      ((eq :completed (idempotency-entry-state entry))
       (values :replay entry))
      (t
       (values :in-progress entry)))))

(defmethod update-entry ((engine memory-idempotency-engine) key fingerprint response ttl-seconds now)
  (let* ((table (memory-idempotency-engine-table engine))
         (entry (gethash key table)))
    (when (%expired-p entry now)
      (remhash key table)
      (setf entry nil))
    (when (and entry
               (eq :processing (idempotency-entry-state entry))
               (string= fingerprint (idempotency-entry-fingerprint entry)))
      (setf (idempotency-entry-state entry) :completed
            (idempotency-entry-response entry) response
            (idempotency-entry-expires-at entry) (+ now ttl-seconds))
      t)))

(defmethod delete-entry ((engine memory-idempotency-engine) key fingerprint)
  (let* ((table (memory-idempotency-engine-table engine))
         (entry (gethash key table)))
    (when (and entry
               (eq :processing (idempotency-entry-state entry))
               (string= fingerprint (idempotency-entry-fingerprint entry)))
      (remhash key table)
      t)))
