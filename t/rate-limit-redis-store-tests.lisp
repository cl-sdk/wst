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
     (lambda (command &rest args)
       (cond
         ((string= command "HMGET")
          (destructuring-bind (key field1 field2) args
            (let ((entry (gethash key records)))
              (list (and entry (getf entry (intern (string-upcase field1) :keyword)))
                    (and entry (getf entry (intern (string-upcase field2) :keyword)))))))
         ((string= command "HSET")
          (destructuring-bind (key field1 value1 field2 value2) args
            (setf (gethash key records)
                  (list (intern (string-upcase field1) :keyword) value1
                        (intern (string-upcase field2) :keyword) value2))
            1))
         ((string= command "EXPIRE")
          (destructuring-bind (key ttl) args
            (setf (gethash key expiries) ttl)
            1))
         ((string= command "DEL")
          (destructuring-bind (key) args
            (let ((removed (if (gethash key records) 1 0)))
              (remhash key records)
              (remhash key expiries)
              removed)))
         (t
          (error "Unknown Redis command in test fake: ~a" command))))
     records
     expiries)))

(5am:def-test redis-rate-limit-store-roundtrip ()
  (multiple-value-bind (command-fn _records _expiries)
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
  (multiple-value-bind (command-fn _records expiries)
      (make-rate-limit-fake-redis)
    (declare (ignore _records))
    (let ((store (make-instance 'io.github.cl-sdk.wst.rate-limit.redis-store:redis-store
                                :command-fn command-fn
                                :window-seconds 42)))
      (io.github.cl-sdk.wst.rate-limit.store:save-window store :client-b 1 2000)
      (5am:is (= 42 (gethash "wst:rate-limit::CLIENT-B" expiries)))))) ; key uses WRITE-TO-STRING on :CLIENT-B
