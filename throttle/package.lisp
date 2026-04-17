(defpackage #:wst.throttle
  (:use #:cl)
  (:documentation "Fixed-window rate limiting and throttling middleware for wst.

Provides:

  • RATE-LIMIT
    Creates a before-middleware function that enforces a fixed-window request budget.

    Syntax:
      (rate-limit &key max-requests window-seconds key-fn on-throttle)

    - :MAX-REQUESTS   – Maximum requests allowed within the window (default: 60).
    - :WINDOW-SECONDS – Length of the time window in seconds (default: 60).
    - :KEY-FN         – Function of one argument (request) that returns a key
                        identifying the client or bucket. Defaults to a shared
                        global bucket (:global) for all clients.
    - :ON-THROTTLE    – Function called as (request response retry-after-seconds)
                        when the budget is exhausted. Must return the response
                        object. Defaults to a 429 Too Many Requests response
                        that includes a Retry-After header.

    Accepted requests automatically receive rate-limit budget headers:
      X-RateLimit-Limit     – The configured maximum.
      X-RateLimit-Remaining – Remaining requests in the current window.
      X-RateLimit-Reset     – Seconds until the window resets.

    Throttled requests receive:
      Retry-After – Seconds until the client may retry.

    Behavior:
      Returns a closure compatible with the WST DSL :before handler protocol.
      The closure produces either (:continue . response) or (:halt . response).")
  (:import-from #:wst.routing
                #:too-many-requests-response
                #:response-headers)
  (:export
   #:rate-limit))

(in-package #:wst.throttle)

(defun %fixed-window-state (state key now window-seconds)
  "Return (count start) for KEY, resetting the window if it has expired."
  (destructuring-bind (count start)
      (gethash key state (list 0 now))
    (if (>= (- now start) window-seconds)
        (list 0 now)
        (list count start))))

(defun rate-limit (&key
                     (max-requests 60)
                     (window-seconds 60)
                     (key-fn (lambda (request)
                               (declare (ignorable request))
                               :global))
                     (on-throttle
                      (lambda (request response retry-after-seconds)
                        (declare (ignorable request))
                        (too-many-requests-response t response
                                                    :headers (list :retry-after
                                                                   (format nil "~a" retry-after-seconds))))))
  "Creates a before-middleware function that enforces fixed-window rate limiting.

Returns middleware compatible with WST DSL `:before` handlers, producing either:
- (:continue . response) when the request is accepted.
- (:halt . response) when the budget is exhausted."
  (let ((state (make-hash-table :test #'equal)))
    (lambda (request response)
      (let* ((key (funcall key-fn request))
             (now (get-universal-time))
             (window (%fixed-window-state state key now window-seconds))
             (count (first window))
             (start (second window))
             (retry-after-seconds (max 0 (- window-seconds (- now start))))
             (remaining (max 0 (- max-requests (1+ count)))))
        (if (>= count max-requests)
            (cons :halt (funcall on-throttle request response retry-after-seconds))
            (progn
              (setf (gethash key state) (list (1+ count) start))
              (setf (response-headers response)
                    (append (response-headers response)
                            (list :x-ratelimit-limit (format nil "~a" max-requests)
                                  :x-ratelimit-remaining (format nil "~a" remaining)
                                  :x-ratelimit-reset (format nil "~a" retry-after-seconds))))
              (cons :continue response)))))))
