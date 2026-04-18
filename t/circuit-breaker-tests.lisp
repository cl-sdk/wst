(in-package :wst.routing.test)

;;;
;;; wst.circuit-breaker suite (pure state machine, no HTTP)
;;;

(5am:def-suite wst.circuit-breaker.suite
  :description "Tests for the pure wst.circuit-breaker package.")

(5am:in-suite wst.circuit-breaker.suite)

(5am:def-test circuit-breaker-starts-in-closed-state ()
  (let ((cb (wst.circuit-breaker:make-circuit-breaker :failure-threshold 2 :recovery-timeout 10)))
    (5am:is (eq :allowed (wst.circuit-breaker:circuit-breaker-check cb)))))

(5am:def-test circuit-breaker-opens-after-failure-threshold-pure ()
  (let ((cb (wst.circuit-breaker:make-circuit-breaker :failure-threshold 2 :recovery-timeout 10)))
    (wst.circuit-breaker:circuit-breaker-record cb t)
    (5am:is (eq :allowed (wst.circuit-breaker:circuit-breaker-check cb)))
    (wst.circuit-breaker:circuit-breaker-record cb t)
    (5am:is (eq :blocked (wst.circuit-breaker:circuit-breaker-check cb)))))

(5am:def-test circuit-breaker-success-resets-failure-count ()
  (let ((cb (wst.circuit-breaker:make-circuit-breaker :failure-threshold 2 :recovery-timeout 10)))
    (wst.circuit-breaker:circuit-breaker-record cb t)
    (wst.circuit-breaker:circuit-breaker-record cb nil)
    (wst.circuit-breaker:circuit-breaker-record cb t)
    (5am:is (eq :allowed (wst.circuit-breaker:circuit-breaker-check cb)))))

(5am:def-test circuit-breaker-transitions-to-half-open-after-timeout ()
  (let* ((now 0)
         (cb (wst.circuit-breaker:make-circuit-breaker
              :failure-threshold 1
              :recovery-timeout 10
              :clock (lambda () now))))
    (wst.circuit-breaker:circuit-breaker-record cb t)
    (5am:is (eq :blocked (wst.circuit-breaker:circuit-breaker-check cb)))
    (setf now 11)
    (5am:is (eq :allowed (wst.circuit-breaker:circuit-breaker-check cb)))))

(5am:def-test circuit-breaker-closes-on-half-open-success ()
  (let* ((now 0)
         (cb (wst.circuit-breaker:make-circuit-breaker
              :failure-threshold 1
              :recovery-timeout 10
              :clock (lambda () now))))
    (wst.circuit-breaker:circuit-breaker-record cb t)
    (setf now 11)
    (wst.circuit-breaker:circuit-breaker-check cb)
    (wst.circuit-breaker:circuit-breaker-record cb nil)
    (5am:is (eq :allowed (wst.circuit-breaker:circuit-breaker-check cb)))))

(5am:def-test circuit-breaker-reopens-on-half-open-failure ()
  (let* ((now 0)
         (cb (wst.circuit-breaker:make-circuit-breaker
              :failure-threshold 1
              :recovery-timeout 10
              :clock (lambda () now))))
    (wst.circuit-breaker:circuit-breaker-record cb t)
    (setf now 11)
    (wst.circuit-breaker:circuit-breaker-check cb)
    (wst.circuit-breaker:circuit-breaker-record cb t)
    (5am:is (eq :blocked (wst.circuit-breaker:circuit-breaker-check cb)))))
