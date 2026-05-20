(defpackage #:io.github.cl-sdk.wst.idempotency.memory-store
  (:use #:cl #:io.github.cl-sdk.wst.idempotency.store)
  (:documentation "In-memory idempotency backend.

Suitable for single-process scenarios; not thread-safe.")
  (:export
   #:memory-store))

(in-package #:io.github.cl-sdk.wst.idempotency.memory-store)

(defclass memory-store ()
  ((table :initform (make-hash-table :test #'equal)
          :reader memory-store-table))
  (:documentation "Hash-table-backed idempotency store.
Not thread-safe."))

(defun %expired-p (entry now)
  (and entry
       (idempotency-entry-expires-at entry)
       (>= now (idempotency-entry-expires-at entry))))

(defmethod claim-idempotency ((store memory-store) key fingerprint ttl-seconds now)
  (let* ((table (memory-store-table store))
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

(defmethod complete-idempotency ((store memory-store) key fingerprint response ttl-seconds now)
  (let* ((table (memory-store-table store))
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

(defmethod release-idempotency ((store memory-store) key fingerprint)
  (let* ((table (memory-store-table store))
         (entry (gethash key table)))
    (when (and entry
               (eq :processing (idempotency-entry-state entry))
               (string= fingerprint (idempotency-entry-fingerprint entry)))
      (remhash key table)
      t)))

