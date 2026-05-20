(defpackage :io.github.cl-sdk.wst.rate-limit.redis-store.test
  (:use #:cl))

(in-package :io.github.cl-sdk.wst.rate-limit.redis-store.test)

(5am:def-suite wst.rate-limit.redis-store.suite
  :description "Tests for the wst.rate-limit.redis-store package.")

(5am:in-suite wst.rate-limit.redis-store.suite)

(defun ensure-rate-limit-entry (records key)
  (or (gethash key records)
      (setf (gethash key records) (make-hash-table :test #'equal))))

(defmacro with-mocked-rate-limit-redis ((records expiries) &body body)
  `(let* ((hmget-original (symbol-function 'redis:red-hmget))
          (hmset-original (symbol-function 'redis:red-hmset))
          (expire-original (symbol-function 'redis:red-expire))
          (del-original (symbol-function 'redis:red-del)))
     (unwind-protect
          (progn
            (setf (symbol-function 'redis:red-hmget)
                  (lambda (key field &rest fields)
                    (let* ((entry (gethash key ,records))
                           (wanted-fields (cons field fields)))

                      (mapcar (lambda (field-name)
                                (and entry (gethash field-name entry)))
                              wanted-fields)))
                  (symbol-function 'redis:red-hmset)
                  (lambda (key &rest fields-and-values)
                    (let ((entry (ensure-rate-limit-entry ,records key)))
                      (loop for (field value) on fields-and-values by #'cddr
                            do (setf (gethash field entry) value)))
                    "OK")
                  (symbol-function 'redis:red-expire)
                  (lambda (key ttl)
                    (setf (gethash key ,expiries) ttl)
                    t)
                  (symbol-function 'redis:red-del)
                  (lambda (key &rest keys)
                    (let ((removed 0))
                      (dolist (k (cons key keys) removed)
                        (when (gethash k ,records)
                          (incf removed))
                        (remhash k ,records)
                        (remhash k ,expiries)))))
            ,@body)
       (setf (symbol-function 'redis:red-hmget) hmget-original
             (symbol-function 'redis:red-hmset) hmset-original
             (symbol-function 'redis:red-expire) expire-original
             (symbol-function 'redis:red-del) del-original))))

(5am:def-test redis-rate-limit-store-roundtrip ()
  (let ((records (make-hash-table :test #'equal))
        (expiries (make-hash-table :test #'equal)))
    (with-mocked-rate-limit-redis (records expiries)
      (let ((store (make-instance 'io.github.cl-sdk.wst.rate-limit.redis-store:redis-store)))
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
          (5am:is-false start))))))

(5am:def-test redis-rate-limit-store-applies-expiry-when-configured ()
  (let ((records (make-hash-table :test #'equal))
        (expiries (make-hash-table :test #'equal)))
    (with-mocked-rate-limit-redis (records expiries)
      (let ((store (make-instance 'io.github.cl-sdk.wst.rate-limit.redis-store:redis-store
                                  :window-seconds 42)))
        (io.github.cl-sdk.wst.rate-limit.store:save-window store :client-b 1 2000)
        (5am:is (= 42 (gethash "wst:rate-limit::CLIENT-B" expiries)))))))
