(defpackage :io.github.cl-sdk.wst.rate-limit.redis-store.test
  (:use #:cl))

(in-package :io.github.cl-sdk.wst.rate-limit.redis-store.test)

(5am:def-suite wst.rate-limit.redis-store.suite
  :description "Tests for the wst.rate-limit.redis-store package.")

(5am:in-suite wst.rate-limit.redis-store.suite)

(defun make-rate-limit-fake-redis ()
  (let ((records (make-hash-table :test #'equal))
        (expiries (make-hash-table :test #'equal)))
    (values
     records
     expiries)))

(defmacro with-mocked-rate-limit-redis ((records expiries) &body body)
  `(let* ((hmget-original (symbol-function 'redis:hmget))
          (hset-original (symbol-function 'redis:hset))
          (expire-original (symbol-function 'redis:expire))
          (del-original (symbol-function 'redis:del)))
     (unwind-protect
          (progn
            (setf (symbol-function 'redis:hmget)
                  (lambda (key field &rest fields)
                    (declare (ignore fields))
                    (let ((entry (gethash key ,records)))
                      (list (and entry (getf entry (intern (string-upcase field) :keyword)))
                            (and entry (getf entry :START)))))
                  (symbol-function 'redis:hset)
                  (lambda (key field value)
                    (setf (gethash key ,records)
                          (list* (intern (string-upcase field) :keyword) value
                                 (or (gethash key ,records) nil)))
                    t)
                  (symbol-function 'redis:expire)
                  (lambda (key ttl)
                    (setf (gethash key ,expiries) ttl)
                    t)
                  (symbol-function 'redis:del)
                  (lambda (key &rest keys)
                    (declare (ignore keys))
                    (let ((removed (if (gethash key ,records) 1 0)))
                      (remhash key ,records)
                      (remhash key ,expiries)
                      removed)))
            ,@body)
       (setf (symbol-function 'redis:hmget) hmget-original
             (symbol-function 'redis:hset) hset-original
             (symbol-function 'redis:expire) expire-original
             (symbol-function 'redis:del) del-original))))

(5am:def-test redis-rate-limit-store-roundtrip ()
  (multiple-value-bind (records expiries)
      (make-rate-limit-fake-redis)
    (declare (ignore _records _expiries))
    (let ((store (make-instance 'io.github.cl-sdk.wst.rate-limit.redis-store:redis-store
                                :command-fn command-fn)))
      (multiple-value-bind (count start)
          (io.github.cl-sdk.wst.rate-limit.store:fetch-window store :client-a)
        (5am:is-false count)
        (5am:is-false start))
      (io.github.cl-sdk.wst.rate-limit.store:save-window store :client-a 3 1000)
      (multiple-value-bind (count start)
          (io.github.cl-sdk.wst.rate-limit.store:fetch-window store :client-a)
        (5am:is (= 3 count))
        (5am:is (= 1000 start)))
      (io.github.cl-sdk.wst.rate-limit.store:delete-window store :client-a)
      (multiple-value-bind (count start)
          (io.github.cl-sdk.wst.rate-limit.store:fetch-window store :client-a)
        (5am:is-false count)
        (5am:is-false start)))))

(5am:def-test redis-rate-limit-store-applies-expiry-when-configured ()
  (multiple-value-bind (records expiries)
      (make-rate-limit-fake-redis)
    (declare (ignore _records))
    (let ((store (make-instance 'io.github.cl-sdk.wst.rate-limit.redis-store:redis-store
                                :command-fn command-fn
                                :window-seconds 42)))
      (io.github.cl-sdk.wst.rate-limit.store:save-window store :client-b 1 2000)
      (5am:is (= 42 (gethash "wst:rate-limit::CLIENT-B" expiries)))))) ; key uses WRITE-TO-STRING on :CLIENT-B
