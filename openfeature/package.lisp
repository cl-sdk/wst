(defpackage #:io.github.cl-sdk.wst.openfeature
  (:use #:cl)
  (:documentation "OpenFeature-style feature flag primitives for wst.

MVP implemented:
- Global API provider and domain provider registry
- Client creation and domain-aware provider resolution
- Evaluation context merge (API -> client -> invocation)
- Typed evaluations: boolean/string/number/object
- Evaluation details with reason and error metadata

Planned for later phases:
- Hooks pipeline
- Provider events / status
- Extended lifecycle and transaction/request propagation")
  (:export
   #:provider
   #:noop-provider
   #:provider-name
   #:provider-metadata
   #:initialize-provider
   #:shutdown-provider
   #:resolve-boolean-details
   #:resolve-string-details
   #:resolve-number-details
   #:resolve-object-details
   #:openfeature-client
   #:openfeature-client-name
   #:openfeature-client-domain
   #:openfeature-client-evaluation-context
   #:make-client
   #:create-client
   #:set-provider
   #:get-provider
   #:set-evaluation-context
   #:get-evaluation-context
   #:merge-evaluation-contexts
   #:evaluation-details
   #:make-evaluation-details
   #:evaluation-details-flag-key
   #:evaluation-details-value
   #:evaluation-details-variant
   #:evaluation-details-reason
   #:evaluation-details-error-code
   #:evaluation-details-error-message
   #:evaluation-details-metadata
   #:get-boolean-value
   #:get-string-value
   #:get-number-value
   #:get-object-value
   #:get-boolean-details
   #:get-string-details
   #:get-number-details
   #:get-object-details
   #:reset-openfeature
   #:*reason-default*
   #:*reason-static*
   #:*reason-error*
   #:*error-provider-not-ready*
   #:*error-flag-not-found*
   #:*error-type-mismatch*
   #:*error-general*))

(in-package #:io.github.cl-sdk.wst.openfeature)

(defparameter *reason-default* :default)
(defparameter *reason-static* :static)
(defparameter *reason-error* :error)

(defparameter *error-provider-not-ready* :provider-not-ready)
(defparameter *error-flag-not-found* :flag-not-found)
(defparameter *error-type-mismatch* :type-mismatch)
(defparameter *error-general* :general)

(defclass provider ()
  ((name :initarg :name :accessor provider-name :initform "provider"))
  (:documentation "Base provider protocol class."))

(defclass noop-provider (provider) ()
  (:documentation "Default provider that always returns fallback values."))

(defgeneric provider-metadata (provider)
  (:documentation "Return metadata for PROVIDER as a plist."))

(defgeneric initialize-provider (provider)
  (:documentation "Initialize PROVIDER lifecycle."))

(defgeneric shutdown-provider (provider)
  (:documentation "Shutdown PROVIDER lifecycle."))

(defgeneric resolve-boolean-details (provider flag-key default-value evaluation-context)
  (:documentation "Resolve boolean flag details."))

(defgeneric resolve-string-details (provider flag-key default-value evaluation-context)
  (:documentation "Resolve string flag details."))

(defgeneric resolve-number-details (provider flag-key default-value evaluation-context)
  (:documentation "Resolve number flag details."))

(defgeneric resolve-object-details (provider flag-key default-value evaluation-context)
  (:documentation "Resolve object flag details."))

(defstruct evaluation-details
  "Evaluation detail record compatible with OpenFeature-style metadata."
  flag-key
  value
  variant
  reason
  error-code
  error-message
  metadata)

(defstruct openfeature-client
  "A client has optional DOMAIN and per-client EVALUATION-CONTEXT."
  (name "client" :type string)
  domain
  (evaluation-context nil :type list))

(defparameter *default-provider* (make-instance 'noop-provider :name "noop"))
(defparameter *domain-providers* (make-hash-table :test 'equal))
(defparameter *api-evaluation-context* nil)

(defun %plist-even-p (plist)
  (and (listp plist)
       (evenp (length plist))))

(defun %ensure-context (context where)
  (unless (%plist-even-p context)
    (error "~A must be a plist (even-length list)." where))
  context)

(defun merge-evaluation-contexts (&rest contexts)
  "Merge context plists left-to-right so later contexts override earlier keys."
  (let ((result nil))
    (dolist (context contexts result)
      (when context
        (%ensure-context context "evaluation context")
        (loop :for (key value) :on context :by #'cddr
              :do (setf (getf result key) value))))))

(defun set-evaluation-context (context)
  "Set global API evaluation context."
  (setf *api-evaluation-context* (%ensure-context context "global evaluation context")))

(defun get-evaluation-context ()
  "Return global API evaluation context."
  *api-evaluation-context*)

(defmethod provider-metadata ((provider provider))
  (list :name (provider-name provider)))

(defmethod initialize-provider ((provider provider))
  provider)

(defmethod shutdown-provider ((provider provider))
  provider)

(defun %default-details (flag-key default-value &key
                          (reason *reason-default*)
                          error-code
                          error-message
                          metadata)
  (make-evaluation-details :flag-key flag-key
                           :value default-value
                           :reason reason
                           :error-code error-code
                           :error-message error-message
                           :metadata metadata))

(defmethod resolve-boolean-details ((provider provider) flag-key default-value evaluation-context)
  (declare (ignore provider evaluation-context))
  (%default-details flag-key default-value
                    :reason *reason-error*
                    :error-code *error-provider-not-ready*
                    :error-message "Provider does not implement boolean resolution."))

(defmethod resolve-string-details ((provider provider) flag-key default-value evaluation-context)
  (declare (ignore provider evaluation-context))
  (%default-details flag-key default-value
                    :reason *reason-error*
                    :error-code *error-provider-not-ready*
                    :error-message "Provider does not implement string resolution."))

(defmethod resolve-number-details ((provider provider) flag-key default-value evaluation-context)
  (declare (ignore provider evaluation-context))
  (%default-details flag-key default-value
                    :reason *reason-error*
                    :error-code *error-provider-not-ready*
                    :error-message "Provider does not implement number resolution."))

(defmethod resolve-object-details ((provider provider) flag-key default-value evaluation-context)
  (declare (ignore provider evaluation-context))
  (%default-details flag-key default-value
                    :reason *reason-error*
                    :error-code *error-provider-not-ready*
                    :error-message "Provider does not implement object resolution."))

(defun reset-openfeature ()
  "Reset global OpenFeature API state."
  (setf *default-provider* (make-instance 'noop-provider :name "noop")
        *domain-providers* (make-hash-table :test 'equal)
        *api-evaluation-context* nil))

(defun set-provider (provider &key domain)
  "Set PROVIDER globally or for a DOMAIN."
  (check-type provider provider)
  (initialize-provider provider)
  (if domain
      (setf (gethash domain *domain-providers*) provider)
      (setf *default-provider* provider))
  provider)

(defun get-provider (&optional domain)
  "Get provider for DOMAIN if present; otherwise default provider."
  (or (and domain (gethash domain *domain-providers*))
      *default-provider*))

(defun make-client (&key (name "client") domain evaluation-context)
  "Create an OpenFeature client."
  (make-openfeature-client :name name
                           :domain domain
                           :evaluation-context (%ensure-context evaluation-context "client evaluation context")))

(defun create-client (&key (name "client") domain evaluation-context)
  "Alias for MAKE-CLIENT."
  (make-client :name name :domain domain :evaluation-context evaluation-context))

(defun %resolve-provider (client)
  (get-provider (openfeature-client-domain client)))

(defun %type-ok-p (kind value)
  (case kind
    (:boolean (or (eq value t) (null value)))
    (:string (stringp value))
    (:number (numberp value))
    (:object t)
    (t nil)))

(defun %resolver-for-kind (kind)
  (ecase kind
    (:boolean #'resolve-boolean-details)
    (:string #'resolve-string-details)
    (:number #'resolve-number-details)
    (:object #'resolve-object-details)))

(defun %evaluate-details (client kind flag-key default-value invocation-context)
  (let* ((provider (%resolve-provider client))
         (context (merge-evaluation-contexts
                   *api-evaluation-context*
                   (openfeature-client-evaluation-context client)
                   invocation-context))
         (resolver (%resolver-for-kind kind)))
    (handler-case
        (let ((details (funcall resolver provider flag-key default-value context)))
          (if (and (typep details 'evaluation-details)
                   (%type-ok-p kind (evaluation-details-value details)))
              details
              (%default-details flag-key default-value
                                :reason *reason-error*
                                :error-code *error-type-mismatch*
                                :error-message "Provider returned invalid details/value shape.")))
      (error (err)
        (%default-details flag-key default-value
                          :reason *reason-error*
                          :error-code *error-general*
                          :error-message (princ-to-string err))))))

(defun get-boolean-details (client flag-key default-value &key evaluation-context)
  (%evaluate-details client :boolean flag-key default-value evaluation-context))

(defun get-string-details (client flag-key default-value &key evaluation-context)
  (%evaluate-details client :string flag-key default-value evaluation-context))

(defun get-number-details (client flag-key default-value &key evaluation-context)
  (%evaluate-details client :number flag-key default-value evaluation-context))

(defun get-object-details (client flag-key default-value &key evaluation-context)
  (%evaluate-details client :object flag-key default-value evaluation-context))

(defun get-boolean-value (client flag-key default-value &key evaluation-context)
  (evaluation-details-value
   (get-boolean-details client flag-key default-value :evaluation-context evaluation-context)))

(defun get-string-value (client flag-key default-value &key evaluation-context)
  (evaluation-details-value
   (get-string-details client flag-key default-value :evaluation-context evaluation-context)))

(defun get-number-value (client flag-key default-value &key evaluation-context)
  (evaluation-details-value
   (get-number-details client flag-key default-value :evaluation-context evaluation-context)))

(defun get-object-value (client flag-key default-value &key evaluation-context)
  (evaluation-details-value
   (get-object-details client flag-key default-value :evaluation-context evaluation-context)))
