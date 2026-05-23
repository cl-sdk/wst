(in-package :io.github.cl-sdk.wst.test)

(5am:def-suite wst.idempotency.suite
  :description "Tests for the wst.idempotency package.")

(5am:in-suite wst.idempotency.suite)

(defclass test-idempotency-engine (io.github.cl-sdk.wst.idempotency:idempotency-engine)
  ((calls :initform nil
          :accessor test-idempotency-engine-calls)
   (custom-storage :initarg :custom-storage
                   :reader test-idempotency-engine-custom-storage)))

(defmethod io.github.cl-sdk.wst.idempotency.store:create-entry ((engine test-idempotency-engine)
                                                                key
                                                                fingerprint
                                                                ttl-seconds
                                                                now)
  (declare (ignore key fingerprint ttl-seconds now))
  (push :create (test-idempotency-engine-calls engine))
  (values :started nil))

(defmethod io.github.cl-sdk.wst.idempotency.store:update-entry ((engine test-idempotency-engine)
                                                                key
                                                                fingerprint
                                                                response
                                                                ttl-seconds
                                                                now)
  (declare (ignore key fingerprint response ttl-seconds now))
  (push :update (test-idempotency-engine-calls engine))
  t)

(defmethod io.github.cl-sdk.wst.idempotency.store:delete-entry ((engine test-idempotency-engine)
                                                                key
                                                                fingerprint)
  (declare (ignore key fingerprint))
  (push :delete (test-idempotency-engine-calls engine))
  t)

(5am:def-test valid-idempotency-key-rejects-empty-and-too-long ()
  (5am:is-false (io.github.cl-sdk.wst.idempotency:valid-idempotency-key-p nil))
  (5am:is-false (io.github.cl-sdk.wst.idempotency:valid-idempotency-key-p ""))
  (5am:is-true (io.github.cl-sdk.wst.idempotency:valid-idempotency-key-p "short-key"))
  (5am:is-false (io.github.cl-sdk.wst.idempotency:valid-idempotency-key-p (make-string 256 :initial-element #\a))))

(5am:def-test main-api-register-store-response-works ()
  (let* ((now 100)
         (engine (make-instance 'io.github.cl-sdk.wst.idempotency.memory-store:memory-idempotency-engine
                                :clock (lambda () now)))
         (scope "checkout")
         (key "k-main")
         (fingerprint "f-main")
         (cached (io.github.cl-sdk.wst.idempotency:make-cached-response
                  :status 200
                  :headers nil
                  :content "ok")))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine scope key fingerprint)
      (declare (ignore replayed))
      (5am:is (eq :started decision)))
    (5am:is-true (io.github.cl-sdk.wst.idempotency:store-response engine scope key fingerprint cached))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine scope key fingerprint)
      (5am:is (eq :replay decision))
      (5am:is (string= "ok" (io.github.cl-sdk.wst.idempotency:cached-response-content replayed))))))

(5am:def-test register-store-then-replay ()
  (let* ((now 100)
         (engine (make-instance 'io.github.cl-sdk.wst.idempotency.memory-store:memory-idempotency-engine
                                :clock (lambda () now)))
         (scope "checkout")
         (key "k1")
         (fingerprint "f1")
         (cached (io.github.cl-sdk.wst.idempotency:make-cached-response
                  :status 200
                  :headers '(:content-type "text/plain")
                  :content "ok")))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine scope key fingerprint)
      (declare (ignore replayed))
      (5am:is (eq :started decision)))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine scope key fingerprint)
      (declare (ignore replayed))
      (5am:is (eq :in-progress decision)))
    (5am:is-true (io.github.cl-sdk.wst.idempotency:store-response
                  engine scope key fingerprint cached))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine scope key fingerprint)
      (5am:is (eq :replay decision))
      (5am:is (= 200 (io.github.cl-sdk.wst.idempotency:cached-response-status replayed)))
      (5am:is (string= "ok" (io.github.cl-sdk.wst.idempotency:cached-response-content replayed))))))

(5am:def-test key-conflict-when-fingerprint-differs ()
  (let ((engine (make-instance 'io.github.cl-sdk.wst.idempotency.memory-store:memory-idempotency-engine)))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine "scope" "same-key" "fingerprint-a")
      (declare (ignore replayed))
      (5am:is (eq :started decision)))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine "scope" "same-key" "fingerprint-b")
      (declare (ignore replayed))
      (5am:is (eq :conflict decision)))))

(5am:def-test drop-request-releases-key ()
  (let* ((engine (make-instance 'io.github.cl-sdk.wst.idempotency.memory-store:memory-idempotency-engine))
         (scope "scope")
         (key "key")
         (fingerprint "f"))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine scope key fingerprint)
      (declare (ignore replayed))
      (5am:is (eq :started decision)))
    (5am:is-true (io.github.cl-sdk.wst.idempotency:drop-request
                  engine scope key fingerprint))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine scope key fingerprint)
      (declare (ignore replayed))
      (5am:is (eq :started decision)))))

(5am:def-test register-store-drop-can-be-specialized-on-engine ()
  (let* ((storage (make-hash-table))
         (engine (make-instance 'test-idempotency-engine
                                :custom-storage storage)))
    (multiple-value-bind (decision replayed)
        (io.github.cl-sdk.wst.idempotency:register-request engine "scope" "key" "fingerprint")
      (declare (ignore replayed))
      (5am:is (eq :started decision)))
    (5am:is-true (io.github.cl-sdk.wst.idempotency:store-response
                  engine "scope" "key" "fingerprint"
                  (io.github.cl-sdk.wst.idempotency:make-cached-response
                   :status 200
                   :headers nil
                   :content "ok")))
    (5am:is-true (io.github.cl-sdk.wst.idempotency:drop-request
                  engine "scope" "key" "fingerprint"))
    (5am:is (hash-table-p (test-idempotency-engine-custom-storage engine)))
    (5am:is (eq storage (test-idempotency-engine-custom-storage engine)))
    (5am:is (equal '(:delete :update :create)
                   (test-idempotency-engine-calls engine)))))
