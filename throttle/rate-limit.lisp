(defpackage #:io.github.cl-sdk.wst.rate-limit
  (:use #:cl)
  (:documentation "Fixed-window rate limiting.

This package is independent of HTTP request and response objects.
It provides a pure rate-limiting primitive that tracks counts per
arbitrary key and can be composed with any middleware layer.

The tracking state is held by a pluggable storage backend that
implements the wst.rate-limit.store protocol (fetch-window / save-window
/ delete-window). A built-in MEMORY-STORE is provided by the
wst.rate-limit.memory-store package.

Provides:

  • RATE-LIMIT
    Creates a fixed-window rate-limiter closure.

    Syntax:
      (rate-limit &key max-requests window-seconds store)

    - :MAX-REQUESTS   – Maximum calls allowed within the window (default: 60).
    - :WINDOW-SECONDS – Length of the time window in seconds (default: 60).
    - :STORE          – A storage backend implementing wst.rate-limit.store.
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
      (let ((limiter (wst.rate-limit:rate-limit :max-requests 100
                                                :window-seconds 60)))
        (multiple-value-bind (allowed-p retry-after remaining)
            (funcall limiter \"192.0.2.1\")
          (if allowed-p
              (format t \"~a requests left in window.\" remaining)
              (format t \"Rate limited. Retry after ~a seconds.\" retry-after))))")
  (:import-from #:io.github.cl-sdk.wst.rate-limit.store
                #:fetch-window
                #:save-window
                #:delete-window)
  (:import-from #:io.github.cl-sdk.wst.rate-limit.memory-store
                #:memory-store)
  (:export
   #:rate-limit))

(in-package #:io.github.cl-sdk.wst.rate-limit)

;;;
;;; Rate limiter
;;;

(defun rate-limit (&key
                     (max-requests 60)
                     (window-seconds 60)
                     (store nil))
  "Creates a fixed-window rate-limiter closure.

The returned closure accepts a single KEY argument and returns three values:
- ALLOWED-P:           T if the call is within budget; NIL when throttled.
- RETRY-AFTER-SECONDS: Seconds until the current window resets.
- REMAINING:           Calls remaining in the window (0 when throttled).

STORE must implement the wst.rate-limit.store protocol. When NIL (the default),
a fresh MEMORY-STORE is used."
  (let ((store (or store (make-instance 'memory-store))))
    (lambda (key)
    (let ((now (get-universal-time)))
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
                (values t retry-after-seconds remaining)))))))))


