(defpackage #:io.github.cl-sdk.wst.circuit-breaker.routing
  (:use #:cl #:io.github.cl-sdk.wst.circuit-breaker)
  (:documentation "HTTP routing middleware adapter for wst.circuit-breaker.

Integrates the circuit breaker state machine with wst.routing.dsl,
exposing it as a WRAP-compatible middleware pair.

Available constructs:

  • CIRCUIT-BREAKER
    Create a circuit breaker middleware pair for use with WRAP.

    Syntax:
      (circuit-breaker &key failure-threshold recovery-timeout
                            open-status open-content clock failure-p)

    Returns a plist:
      :BEFORE => function suitable for WRAP :BEFORE
      :AFTER  => function suitable for WRAP :AFTER

    Parameters:
    - FAILURE-THRESHOLD: Consecutive failures before opening the circuit (default 5).
    - RECOVERY-TIMEOUT:  Seconds to wait in open state before retrying (default 60).
    - OPEN-STATUS:       HTTP status code returned while the circuit is open (default 503).
    - OPEN-CONTENT:      Response body returned while the circuit is open
                         (default \"service unavailable\").
    - CLOCK:             Zero-argument function returning the current time
                         (default GET-UNIVERSAL-TIME).
    - FAILURE-P:         Predicate accepting an HTTP status code (integer) and returning T
                         if that status should be counted as a failure
                         (default: status >= 500).")
  (:export
   #:circuit-breaker))

(in-package #:io.github.cl-sdk.wst.circuit-breaker.routing)

(defun circuit-breaker (&key
                          (failure-threshold 5)
                          (recovery-timeout 60)
                          (open-status 503)
                          (open-content "service unavailable")
                          (clock #'get-universal-time)
                          (failure-p (lambda (status) (>= status 500))))
  "Create a circuit breaker middleware pair for use with wst.routing.dsl:wrap.

Returns a plist:
  :BEFORE => function suitable for WRAP :BEFORE
  :AFTER  => function suitable for WRAP :AFTER

Parameters:
- FAILURE-THRESHOLD: Consecutive failures before opening the circuit (default 5).
- RECOVERY-TIMEOUT:  Seconds to wait in open state before retrying (default 60).
- OPEN-STATUS:       HTTP status code returned while the circuit is open (default 503).
- OPEN-CONTENT:      Response body returned while the circuit is open
                     (default \"service unavailable\").
- CLOCK:             Zero-argument function returning the current time
                     (default GET-UNIVERSAL-TIME).
- FAILURE-P:         Predicate accepting an HTTP status code (integer) and returning T
                     if that status should be counted as a failure
                     (default: status >= 500).

Circuit state transitions are handled by WST.CIRCUIT-BREAKER."
  (let ((cb (make-circuit-breaker
             :failure-threshold failure-threshold
             :recovery-timeout recovery-timeout
             :clock clock))
        (blocked nil))
    (list
     :before
     (lambda (request response)
       (declare (ignore request))
       (if (eq (circuit-breaker-check cb) :blocked)
           (progn
             (setf blocked t)
             (io.github.cl-sdk.wst.routing:write-response response
                                                          :status open-status
                                                          :content open-content)
             (cons :halt response))
           (progn
             (setf blocked nil)
             (cons :continue response))))
     :after
     (lambda (request response)
       (declare (ignore request))
       (unless blocked
         (circuit-breaker-record
          cb
          (funcall failure-p (io.github.cl-sdk.wst.routing:response-status response))))
       response))))
