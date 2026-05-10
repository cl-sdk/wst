(in-package :io.github.cl-sdk.wst.test)

;;;
;;; wst.rate-limit suite
;;;

(5am:def-suite wst.rate-limit.suite
  :description "Tests for the wst.rate-limit package.")

(5am:in-suite wst.rate-limit.suite)

(5am:def-test rate-limit-allows-calls-up-to-max ()
  (let ((limiter (io.github.cl-sdk.wst.rate-limit:rate-limit :max-requests 3 :window-seconds 60)))
    (multiple-value-bind (a) (funcall limiter :k) (5am:is-true a))
    (multiple-value-bind (a) (funcall limiter :k) (5am:is-true a))
    (multiple-value-bind (a) (funcall limiter :k) (5am:is-true a))
    (multiple-value-bind (a) (funcall limiter :k) (5am:is-false a))))

(5am:def-test rate-limit-returns-remaining-count ()
  (let ((limiter (io.github.cl-sdk.wst.rate-limit:rate-limit :max-requests 3 :window-seconds 60)))
    (multiple-value-bind (allowed-p retry-after remaining)
        (funcall limiter :k)
      (declare (ignore retry-after))
      (5am:is-true allowed-p)
      (5am:is (= 2 remaining)))))

(5am:def-test rate-limit-tracks-keys-independently ()
  (let ((limiter (io.github.cl-sdk.wst.rate-limit:rate-limit :max-requests 1 :window-seconds 60)))
    (funcall limiter :a)
    (multiple-value-bind (allowed-p) (funcall limiter :a) (5am:is-false allowed-p))
    (multiple-value-bind (allowed-p) (funcall limiter :b) (5am:is-true allowed-p))))

;;; A minimal custom store that records which operations were called.
;;; Defined at the top level so DEFCLASS does not pollute a test closure.

(defclass recording-store ()
  ((table :initform (make-hash-table :test #'equal) :reader recording-store-table)
   (calls :initform nil :accessor recording-store-calls)))

(defmethod io.github.cl-sdk.wst.rate-limit.store:fetch-window ((s recording-store) key)
  (push :fetch (recording-store-calls s))
  (let ((entry (gethash key (recording-store-table s))))
    (if entry (values (car entry) (cdr entry)) (values nil nil))))

(defmethod io.github.cl-sdk.wst.rate-limit.store:save-window ((s recording-store) key count start-time)
  (push :save (recording-store-calls s))
  (setf (gethash key (recording-store-table s)) (cons count start-time)))

(defmethod io.github.cl-sdk.wst.rate-limit.store:delete-window ((s recording-store) key)
  (push :delete (recording-store-calls s))
  (remhash key (recording-store-table s)))

(5am:def-test rate-limit-uses-custom-store ()
  (let* ((store (make-instance 'recording-store))
         (limiter (io.github.cl-sdk.wst.rate-limit:rate-limit :max-requests 2 :window-seconds 60 :store store)))
    (funcall limiter "k")
    (funcall limiter "k")
    (5am:is-true (member :fetch (recording-store-calls store)))
    (5am:is-true (member :save (recording-store-calls store)))))

(5am:def-test memory-store-implements-store-protocol ()
  (let ((store (make-instance 'io.github.cl-sdk.wst.rate-limit.memory-store:memory-store)))
    ;; Initially empty
    (multiple-value-bind (count start) (io.github.cl-sdk.wst.rate-limit.store:fetch-window store "k")
      (5am:is-false count)
      (5am:is-false start))
    ;; After saving, the values are retrievable
    (io.github.cl-sdk.wst.rate-limit.store:save-window store "k" 5 1000)
    (multiple-value-bind (count start) (io.github.cl-sdk.wst.rate-limit.store:fetch-window store "k")
      (5am:is (= 5 count))
      (5am:is (= 1000 start)))
    ;; After deleting, the entry is gone
    (io.github.cl-sdk.wst.rate-limit.store:delete-window store "k")
    (multiple-value-bind (count start) (io.github.cl-sdk.wst.rate-limit.store:fetch-window store "k")
      (5am:is-false count)
      (5am:is-false start))))
