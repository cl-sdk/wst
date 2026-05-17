(defpackage #:io.github.cl-sdk.wst.feature-flag.test
  (:use #:cl #:fiveam #:io.github.cl-sdk.wst.feature-flag))

(in-package #:io.github.cl-sdk.wst.feature-flag.test)

(def-suite feature-flag-suite)
(in-suite feature-flag-suite)

(defmacro with-global-evaluation-context ((context) &body body)
  `(let ((previous (get-evaluation-context)))
     (unwind-protect
          (progn
            (set-evaluation-context ,context)
            ,@body)
       (set-evaluation-context previous))))

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

(defclass lifecycle-provider (provider)
  ((initialize-called-p :accessor initialize-called-p :initform nil)
   (shutdown-called-p :accessor shutdown-called-p :initform nil)))

(defmethod initialize-provider ((provider lifecycle-provider))
  (setf (initialize-called-p provider) t)
  provider)

(defmethod shutdown-provider ((provider lifecycle-provider))
  (setf (shutdown-called-p provider) t)
  provider)

(defclass provider-holder ()
  ((default-provider :initarg :default-provider :reader provider-holder-default-provider :initform nil)
   (domain-providers :initarg :domain-providers :reader provider-holder-domain-providers :initform nil)))

(defmethod acquire-client ((provider provider) &key (domain "default"))
  (make-instance 'client :provider provider :domain domain))

(defmethod acquire-client ((holder provider-holder) &key (domain "default"))
  (let ((provider (or (and domain (cdr (assoc domain (provider-holder-domain-providers holder) :test #'equal)))
                      (provider-holder-default-provider holder)
                      (make-instance 'provider :name "default"))))
    (make-instance 'client :provider provider :domain domain)))

(defmethod resolve-provider ((holder provider-holder) domain)
  (or (and domain (cdr (assoc domain (provider-holder-domain-providers holder) :test #'equal)))
      (provider-holder-default-provider holder)
      (call-next-method)))

(test provider-without-resolver-returns-default-and-provider-not-ready-error
  (let* ((client (acquire-client (make-instance 'provider :name "base")))
         (details (get-boolean-details client "flag-a" nil)))
    (is-false (evaluation-details-value details))
    (is (eq *reason-error* (evaluation-details-reason details)))
    (is (eq *error-provider-not-ready* (evaluation-details-error-code details)))))

(test provider-errors-fallback-to-default-with-general-error
  (let* ((holder (make-instance 'provider-holder
                                :default-provider (make-instance 'exploding-provider :name "explode")))
         (client (acquire-client holder))
         (details (get-boolean-details client "flag-a" t)))
    (is-true (evaluation-details-value details))
    (is (eq *reason-error* (evaluation-details-reason details)))
    (is (eq *error-general* (evaluation-details-error-code details)))))

(test resolve-provider-does-not-trigger-lifecycle
  (let ((provider (make-instance 'lifecycle-provider :name "lifecycle")))
    (resolve-provider (make-instance 'provider-holder :default-provider provider) nil)
    (is-false (initialize-called-p provider))
    (is-false (shutdown-called-p provider))))

(test evaluation-context-merges-api-client-and-invocation-with-right-precedence
  (with-global-evaluation-context ((list :shared :api :api-only 1))
    (let* ((provider (make-instance 'static-provider :name "static"
                                    :values '(("flag-a" . t))))
           (client (acquire-client provider)))
      (setf (slot-value client 'io.github.cl-sdk.wst.feature-flag::evaluation-context)
            '(:shared :client :client-only 2))
      (is-true (get-boolean-value client "flag-a" nil :evaluation-context '(:shared :call :call-only 3)))
      (let ((ctx (static-provider-last-context provider)))
        (is (eq :call (getf ctx :shared)))
        (is (= 1 (getf ctx :api-only)))
        (is (= 2 (getf ctx :client-only)))
        (is (= 3 (getf ctx :call-only)))))))

(test global-evaluation-context-can-be-set-and-read
  (with-global-evaluation-context ((list :tenant "acme"))
    (is (equal '(:tenant "acme") (get-evaluation-context)))))

(test domain-provider-selection-prefers-domain-over-default
  (let* ((holder (make-instance 'provider-holder
                                :default-provider (make-instance 'static-provider :name "default"
                                                                 :values '(("flag-a" . nil)))
                                :domain-providers (list (cons "payments"
                                                              (make-instance 'static-provider :name "payments"
                                                                             :values '(("flag-a" . t)))))))
         (default-client (acquire-client holder))
         (payments-client (acquire-client holder :domain "payments")))
    (is-false (get-boolean-value default-client "flag-a" nil))
    (is-true (get-boolean-value payments-client "flag-a" nil))))

(test typed-evaluations-return-provider-values-and-details
  (let* ((holder (make-instance 'provider-holder
                                :default-provider (make-instance 'static-provider :name "typed"
                                                                 :values '(("bool-flag" . t)
                                                                           ("str-flag" . "beta")
                                                                           ("num-flag" . 42)
                                                                           ("obj-flag" . (:enabled t))))))
         (client (acquire-client holder)))
    (is-true (get-boolean-value client "bool-flag" nil))
    (is (string= "beta" (get-string-value client "str-flag" "default")))
    (is (= 42 (get-number-value client "num-flag" 0)))
    (is (equal '(:enabled t) (get-object-value client "obj-flag" nil)))
    (let ((details (get-string-details client "str-flag" "default")))
      (is (string= "str-flag" (evaluation-details-flag-key details)))
      (is (eq *reason-static* (evaluation-details-reason details))))))
