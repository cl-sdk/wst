(defpackage #:io.github.cl-sdk.wst.openfeature.test
  (:use #:cl #:fiveam #:io.github.cl-sdk.wst.openfeature))

(in-package #:io.github.cl-sdk.wst.openfeature.test)

(def-suite openfeature-suite)
(in-suite openfeature-suite)

(defmacro with-openfeature-reset (&body body)
  `(unwind-protect
        (progn
          (reset-openfeature)
          ,@body)
     (reset-openfeature)))

(defclass static-provider (provider)
  ((values :initarg :values :reader static-provider-values)
   (last-context :accessor static-provider-last-context :initform nil)))

(defun %lookup-value (provider flag-key)
  (cdr (assoc flag-key (static-provider-values provider) :test #'equal)))

(defmethod resolve-boolean-details ((provider static-provider) flag-key default-value evaluation-context)
  (setf (static-provider-last-context provider) evaluation-context)
  (let ((value (%lookup-value provider flag-key)))
    (if (null value)
        (make-evaluation-details :flag-key flag-key
                                 :value default-value
                                 :reason *reason-default*
                                 :error-code *error-flag-not-found*)
        (make-evaluation-details :flag-key flag-key
                                 :value value
                                 :reason *reason-static*))))

(defmethod resolve-string-details ((provider static-provider) flag-key default-value evaluation-context)
  (setf (static-provider-last-context provider) evaluation-context)
  (let ((value (%lookup-value provider flag-key)))
    (if (stringp value)
        (make-evaluation-details :flag-key flag-key
                                 :value value
                                 :reason *reason-static*)
        (make-evaluation-details :flag-key flag-key
                                 :value default-value
                                 :reason *reason-default*
                                 :error-code *error-flag-not-found*))))

(defmethod resolve-number-details ((provider static-provider) flag-key default-value evaluation-context)
  (setf (static-provider-last-context provider) evaluation-context)
  (let ((value (%lookup-value provider flag-key)))
    (if (numberp value)
        (make-evaluation-details :flag-key flag-key
                                 :value value
                                 :reason *reason-static*)
        (make-evaluation-details :flag-key flag-key
                                 :value default-value
                                 :reason *reason-default*
                                 :error-code *error-flag-not-found*))))

(defmethod resolve-object-details ((provider static-provider) flag-key default-value evaluation-context)
  (setf (static-provider-last-context provider) evaluation-context)
  (let ((value (%lookup-value provider flag-key)))
    (if value
        (make-evaluation-details :flag-key flag-key
                                 :value value
                                 :reason *reason-static*)
        (make-evaluation-details :flag-key flag-key
                                 :value default-value
                                 :reason *reason-default*
                                 :error-code *error-flag-not-found*))))

(defclass exploding-provider (provider) ())

(defmethod resolve-boolean-details ((provider exploding-provider) flag-key default-value evaluation-context)
  (declare (ignore provider flag-key default-value evaluation-context))
  (error "provider exploded"))

(test noop-provider-returns-default-and-metadata-error
  (with-openfeature-reset
    (let* ((client (create-client))
           (details (get-boolean-details client "flag-a" nil)))
      (is-false (evaluation-details-value details))
      (is (eq *reason-error* (evaluation-details-reason details)))
      (is (eq *error-provider-not-ready* (evaluation-details-error-code details))))))

(test provider-errors-fallback-to-default-with-general-error
  (with-openfeature-reset
    (set-provider (make-instance 'exploding-provider :name "explode"))
    (let* ((client (create-client))
           (details (get-boolean-details client "flag-a" t)))
      (is-true (evaluation-details-value details))
      (is (eq *reason-error* (evaluation-details-reason details)))
      (is (eq *error-general* (evaluation-details-error-code details))))))

(test evaluation-context-merges-api-client-and-invocation-with-right-precedence
  (with-openfeature-reset
    (let ((provider (make-instance 'static-provider :name "static"
                                   :values '(("flag-a" . t)))))
      (set-provider provider)
      (set-evaluation-context '(:shared :api :api-only 1))
      (let ((client (create-client :evaluation-context '(:shared :client :client-only 2))))
        (is-true (get-boolean-value client "flag-a" nil :evaluation-context '(:shared :call :call-only 3)))
        (let ((ctx (static-provider-last-context provider)))
          (is (eq :call (getf ctx :shared)))
          (is (= 1 (getf ctx :api-only)))
          (is (= 2 (getf ctx :client-only)))
          (is (= 3 (getf ctx :call-only))))))))

(test domain-provider-selection-prefers-domain-over-default
  (with-openfeature-reset
    (set-provider (make-instance 'static-provider :name "default"
                                 :values '(("flag-a" . nil))))
    (set-provider (make-instance 'static-provider :name "payments"
                                 :values '(("flag-a" . t)))
                  :domain "payments")
    (let ((default-client (create-client))
          (payments-client (create-client :domain "payments")))
      (is-false (get-boolean-value default-client "flag-a" nil))
      (is-true (get-boolean-value payments-client "flag-a" nil)))))

(test typed-evaluations-return-provider-values-and-details
  (with-openfeature-reset
    (set-provider (make-instance 'static-provider :name "typed"
                                 :values '(("bool-flag" . t)
                                           ("str-flag" . "beta")
                                           ("num-flag" . 42)
                                           ("obj-flag" . (:enabled t)))))
    (let ((client (create-client)))
      (is-true (get-boolean-value client "bool-flag" nil))
      (is (string= "beta" (get-string-value client "str-flag" "default")))
      (is (= 42 (get-number-value client "num-flag" 0)))
      (is (equal '(:enabled t) (get-object-value client "obj-flag" nil)))
      (let ((details (get-string-details client "str-flag" "default")))
        (is (string= "str-flag" (evaluation-details-flag-key details)))
        (is (eq *reason-static* (evaluation-details-reason details)))))))
