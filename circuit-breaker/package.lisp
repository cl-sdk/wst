(defpackage #:wst.circuit-breaker
  (:use #:cl)
  (:documentation "Circuit breaker state machine.

All state is held inside the CIRCUIT-BREAKER struct and is passed
explicitly to every function.

Available constructs:

  • MAKE-CIRCUIT-BREAKER
    Create a new circuit breaker.

    Syntax:
      (make-circuit-breaker &key failure-threshold recovery-timeout clock)

    - FAILURE-THRESHOLD – Number of consecutive failures required to open
                          the circuit (default: 5).
    - RECOVERY-TIMEOUT  – Seconds to remain open before entering
                          half-open state (default: 60).
    - CLOCK             – Zero-argument function returning the current time
                          as an integer (default: GET-UNIVERSAL-TIME).
                          Override in tests to control time deterministically.


  • CIRCUIT-BREAKER-CHECK
    Check whether a call should be allowed through the circuit breaker.

    Syntax:
      (circuit-breaker-check cb)

    - CB – A CIRCUIT-BREAKER struct.

    Returns:
      :ALLOWED – The call may proceed.
      :BLOCKED – The circuit is open; the call should be short-circuited.

    Side-effect:
      If the circuit is :OPEN and the recovery timeout has elapsed,
      transitions the circuit to :HALF-OPEN and returns :ALLOWED.


  • CIRCUIT-BREAKER-RECORD
    Record the outcome of a call that was allowed through.

    Syntax:
      (circuit-breaker-record cb failed-p)

    - CB       – A CIRCUIT-BREAKER struct.
    - FAILED-P – T if the call failed, NIL if it succeeded.

    State transitions:
      :CLOSED + failure  → increment counter; open when threshold reached.
      :CLOSED + success  → reset counter (stay closed).
      :HALF-OPEN + success → transition to :CLOSED.
      :HALF-OPEN + failure → transition back to :OPEN.")
  (:export
   #:make-circuit-breaker
   #:circuit-breaker-check
   #:circuit-breaker-record))

(in-package #:wst.circuit-breaker)

(defstruct circuit-breaker
  "Stateful circuit breaker.

Fields:
- FAILURE-THRESHOLD: Number of consecutive failures before opening the circuit.
- RECOVERY-TIMEOUT:  Seconds to wait in open state before entering half-open.
- STATE:             Current circuit state – :closed, :open, or :half-open.
- FAILURES:          Consecutive failure count (reset on success).
- OPENED-AT:         Timestamp (from CLOCK) when the circuit was last opened.
- CLOCK:             Zero-argument function returning the current time."
  (failure-threshold 5 :type integer)
  (recovery-timeout 60 :type integer)
  (state :closed :type symbol)
  (failures 0 :type integer)
  (opened-at nil)
  (clock #'get-universal-time :type function))

(declaim (ftype (function (circuit-breaker) symbol) circuit-breaker-check))
(defun circuit-breaker-check (cb)
  "Check whether a call should be allowed through CB.

Returns :ALLOWED or :BLOCKED.

When the circuit is :OPEN and the recovery timeout has elapsed, transitions
it to :HALF-OPEN so that one probe call can be made."
  (with-slots (state opened-at clock recovery-timeout) cb
    (cond
      ((eq state :open)
       (if (and opened-at
                (>= (- (funcall clock) opened-at) recovery-timeout))
           (progn
             (setf state :half-open)
             :allowed)
           :blocked))
      (t :allowed))))

(declaim (ftype (function (circuit-breaker t) t) circuit-breaker-record))
(defun circuit-breaker-record (cb failed-p)
  "Record the outcome of a call that was allowed through CB.

FAILED-P – T if the call failed, NIL if it succeeded.

Drives all state transitions based on the current circuit state and the
outcome. This function must only be called for calls that were allowed
through (i.e. CIRCUIT-BREAKER-CHECK returned :ALLOWED)."
  (with-slots (state failures failure-threshold opened-at clock) cb
    (cond
      ((eq state :half-open)
       (if failed-p
           (setf state :open
                 opened-at (funcall clock))
           (setf state :closed
                 failures 0
                 opened-at nil)))
      (failed-p
       (incf failures)
       (when (>= failures failure-threshold)
         (setf state :open
               opened-at (funcall clock))))
      (t
       (setf state :closed
             failures 0
             opened-at nil)))))
