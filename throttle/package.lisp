(defpackage #:wst.throttle
  (:use #:cl)
  (:documentation "Fixed-window rate limiting and throttling.

This package is independent of HTTP request and response objects.
It provides a pure rate-limiting primitive that tracks counts per
arbitrary key and can be composed with any middleware layer.

The tracking state is held by a pluggable storage backend that
implements the wst.throttle.store protocol (fetch-window / save-window
/ delete-window). A built-in MEMORY-STORE backed by a hash table is
provided and used by default.

Provides:

  • MEMORY-STORE
    A built-in in-memory storage backend.

    Syntax:
      (make-instance 'memory-store)

    Stores window state in a hash table keyed by EQUAL. Suitable for
    single-process use; not thread-safe.


  • RATE-LIMIT
    Creates a fixed-window rate-limiter closure.

    Syntax:
      (rate-limit &key max-requests window-seconds store)

    - :MAX-REQUESTS   – Maximum calls allowed within the window (default: 60).
    - :WINDOW-SECONDS – Length of the time window in seconds (default: 60).
    - :STORE          – A storage backend implementing wst.throttle.store.
                        Defaults to a fresh MEMORY-STORE.

    Returns a closure of one argument KEY. Calling the closure produces
    three values:
      1. ALLOWED-P            – T if the call is within budget, NIL if throttled.
      2. RETRY-AFTER-SECONDS  – Seconds until the current window resets.
      3. REMAINING            – Remaining calls allowed in the window (0 when
                                throttled).

    The KEY may be any value comparable with EQUAL (or the backend's own
    equality test). Separate keys are tracked independently, allowing
    per-user, per-IP, or any other bucketing strategy.

    Expired entries are lazily evicted from the store on the next access
    for that key, preventing unbounded memory growth.

    Example:
      (let ((limiter (wst.throttle:rate-limit :max-requests 100
                                              :window-seconds 60)))
        (multiple-value-bind (allowed-p retry-after remaining)
            (funcall limiter \"192.0.2.1\")
          (if allowed-p
              (format t \"~a requests left in window.\" remaining)
              (format t \"Rate limited. Retry after ~a seconds.\" retry-after))))")
  (:import-from #:wst.throttle.store
                #:fetch-window
                #:save-window
                #:delete-window)
  (:export
   #:memory-store
   #:rate-limit))

(in-package #:wst.throttle)

;;;
;;; Built-in in-memory storage backend
;;;

(defclass memory-store ()
  ((table :initform (make-hash-table :test #'equal)
          :reader memory-store-table))
  (:documentation "A simple in-memory throttle store backed by a hash table.
Suitable for single-process use; not thread-safe."))

(defmethod wst.throttle.store:fetch-window ((store memory-store) key)
  "Returns (values count start-time) if an entry for KEY exists, (values nil nil) otherwise."
  (let ((entry (gethash key (memory-store-table store))))
    (if entry
        (values (car entry) (cdr entry))
        (values nil nil))))

(defmethod wst.throttle.store:save-window ((store memory-store) key count start-time)
  "Stores COUNT and START-TIME for KEY in the hash table."
  (setf (gethash key (memory-store-table store)) (cons count start-time)))

(defmethod wst.throttle.store:delete-window ((store memory-store) key)
  "Removes the entry for KEY from the hash table."
  (remhash key (memory-store-table store)))

;;;
;;; Rate limiter
;;;

(defun rate-limit (&key
                     (max-requests 60)
                     (window-seconds 60)
                     (store (make-instance 'memory-store)))
  "Creates a fixed-window rate-limiter closure.

The returned closure accepts a single KEY argument and returns three values:
- ALLOWED-P:           T if the call is within budget; NIL when throttled.
- RETRY-AFTER-SECONDS: Seconds until the current window resets.
- REMAINING:           Calls remaining in the window (0 when throttled).

The STORE argument must implement the wst.throttle.store protocol."
  (lambda (key)
    (let* ((now (get-universal-time)))
      (multiple-value-bind (count start)
          (fetch-window store key)
        ;; Lazily evict entries whose window has elapsed.
        ;; >= means the window period is complete; a new one begins now.
        (when (and count (>= (- now start) window-seconds))
          (delete-window store key)
          (setf count nil))
        (unless count
          (setf count 0
                start now))
        (let* ((retry-after-seconds (max 0 (- window-seconds (- now start))))
               (remaining (max 0 (- max-requests (1+ count)))))
          (if (>= count max-requests)
              (values nil retry-after-seconds 0)
              (progn
                (save-window store key (1+ count) start)
                (values t retry-after-seconds remaining))))))))

