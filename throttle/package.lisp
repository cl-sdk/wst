(defpackage #:wst.throttle
  (:use #:cl)
  (:documentation "Fixed-window rate limiting and throttling.

This package is independent of HTTP request and response objects.
It provides a pure rate-limiting primitive that tracks counts per
arbitrary key and can be composed with any middleware layer.

Provides:

  • RATE-LIMIT
    Creates a fixed-window rate-limiter closure.

    Syntax:
      (rate-limit &key max-requests window-seconds)

    - :MAX-REQUESTS   – Maximum calls allowed within the window (default: 60).
    - :WINDOW-SECONDS – Length of the time window in seconds (default: 60).

    Returns a closure of one argument KEY. Calling the closure produces
    three values:
      1. ALLOWED-P            – T if the call is within budget, NIL if throttled.
      2. RETRY-AFTER-SECONDS  – Seconds until the current window resets.
      3. REMAINING            – Remaining calls allowed in the window (0 when
                                throttled).

    The KEY may be any value comparable with EQUAL (e.g. a string, keyword,
    or integer). Separate keys are tracked independently, allowing per-user,
    per-IP, or any other bucketing strategy.

    Expired entries are lazily evicted from the internal table on the next
    access for that key, preventing unbounded memory growth.

    Example:
      (let ((limiter (wst.throttle:rate-limit :max-requests 100
                                              :window-seconds 60)))
        (multiple-value-bind (allowed-p retry-after remaining)
            (funcall limiter \"192.0.2.1\")
          (if allowed-p
              (format t \"~a requests left in window.\" remaining)
              (format t \"Rate limited. Retry after ~a seconds.\" retry-after))))")
  (:export
   #:rate-limit))

(in-package #:wst.throttle)

(defstruct (window-state (:constructor make-window-state (count start)))
  "Holds the per-key fixed-window counter and the timestamp when the window began."
  (count 0 :type integer)
  (start 0 :type integer))

(defun rate-limit (&key (max-requests 60) (window-seconds 60))
  "Creates a fixed-window rate-limiter closure.

The returned closure accepts a single KEY argument and returns three values:
- ALLOWED-P:           T if the call is within budget; NIL when throttled.
- RETRY-AFTER-SECONDS: Seconds until the current window resets.
- REMAINING:           Calls remaining in the window (0 when throttled)."
  (let ((state (make-hash-table :test #'equal)))
    (lambda (key)
      (let* ((now (get-universal-time))
             (existing (gethash key state))
             ;; Lazily evict entries whose window has elapsed.
             ;; >= means the window period is complete; a new one begins now.
             (window (cond
                       ((null existing)
                        (make-window-state 0 now))
                       ((>= (- now (window-state-start existing)) window-seconds)
                        (remhash key state)
                        (make-window-state 0 now))
                       (t existing)))
             (count (window-state-count window))
             (start (window-state-start window))
             (retry-after-seconds (max 0 (- window-seconds (- now start))))
             (remaining (max 0 (- max-requests (1+ count)))))
        (if (>= count max-requests)
            (values nil retry-after-seconds 0)
            (progn
              (setf (gethash key state) (make-window-state (1+ count) start))
              (values t retry-after-seconds remaining)))))))
