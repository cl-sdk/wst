(defpackage #:io.github.cl-sdk.wst.feature-flag
  (:use #:cl)
  (:documentation "Feature-flag primitives for wst.

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
   #:feature-flag-client
   #:feature-flag-client-name
   #:feature-flag-client-domain
   #:feature-flag-client-evaluation-context
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
   #:reset-feature-flag
   #:*reason-default*
   #:*reason-static*
   #:*reason-error*
   #:*error-provider-not-ready*
   #:*error-flag-not-found*
   #:*error-type-mismatch*
   #:*error-general*))

(in-package #:io.github.cl-sdk.wst.feature-flag)

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
  (:documentation "Return metadata for PROVIDER as a plist.
Example:
  (provider-metadata (make-instance 'provider :name \"demo\"))
  => (:name \"demo\")"))

(defgeneric initialize-provider (provider)
  (:documentation "Initialize PROVIDER lifecycle.
Caller-managed: API registration does not invoke this automatically.
Example:
  (initialize-provider (make-instance 'provider :name \"demo\"))
  => #<PROVIDER ...>"))

(defgeneric shutdown-provider (provider)
  (:documentation "Shutdown PROVIDER lifecycle.
Caller-managed: API reset/registration does not invoke this automatically.
Example:
  (shutdown-provider (make-instance 'provider :name \"demo\"))
  => #<PROVIDER ...>"))

(defgeneric resolve-boolean-details (provider flag-key default-value evaluation-context)
  (:documentation "Resolve boolean flag details.
Example:
  (resolve-boolean-details provider \"beta\" nil '(:user-id \"u1\"))
  => #S(EVALUATION-DETAILS ...)"))

(defgeneric resolve-string-details (provider flag-key default-value evaluation-context)
  (:documentation "Resolve string flag details.
Example:
  (resolve-string-details provider \"variant\" \"control\" '(:user-id \"u1\"))
  => #S(EVALUATION-DETAILS ...)"))

(defgeneric resolve-number-details (provider flag-key default-value evaluation-context)
  (:documentation "Resolve number flag details.
Example:
  (resolve-number-details provider \"max-items\" 10 '(:user-id \"u1\"))
  => #S(EVALUATION-DETAILS ...)"))

(defgeneric resolve-object-details (provider flag-key default-value evaluation-context)
  (:documentation "Resolve object flag details.
Example:
  (resolve-object-details provider \"config\" '(:enabled nil) '(:user-id \"u1\"))
  => #S(EVALUATION-DETAILS ...)"))

(defstruct evaluation-details
  "Evaluation detail record for feature-flag metadata."
  flag-key
  value
  variant
  reason
  error-code
  error-message
  metadata)

(defstruct feature-flag-client
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
  "Merge context plists in argument order so later contexts override earlier keys.
Key order in result plists is implementation-dependent.
Example:
  (merge-evaluation-contexts '(:a 1 :shared :api) '(:b 2 :shared :client) '(:c 3 :shared :call))
  => (:a 1 :b 2 :c 3 :shared :call)"
  (let ((result nil))
    (dolist (context contexts result)
      (when context
        (%ensure-context context "evaluation context")
        (loop :for (key value) :on context :by #'cddr
              :do (setf (getf result key) value))))))

(defun set-evaluation-context (context)
  "Set global API evaluation context.
Example:
  (set-evaluation-context '(:region \"eu\"))
  => (:region \"eu\")"
  (setf *api-evaluation-context* (%ensure-context context "global evaluation context")))

(defun get-evaluation-context ()
  "Return global API evaluation context.
Example:
  (get-evaluation-context)
  => (:region \"eu\")"
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

(defun reset-feature-flag ()
  "Reset global feature-flag API state.
Example:
  (reset-feature-flag)
  => NIL"
  (setf *default-provider* (make-instance 'noop-provider :name "noop")
        *domain-providers* (make-hash-table :test 'equal)
        *api-evaluation-context* nil))

(defun set-provider (provider &key domain)
  "Set PROVIDER globally or for a DOMAIN.
Lifecycle is caller-managed (initialize/shutdown are not called automatically).
Example:
  (set-provider (make-instance 'noop-provider :name \"default\") :domain \"payments\")
  => #<NOOP-PROVIDER ...>"
  (check-type provider provider)
  (if domain
      (setf (gethash domain *domain-providers*) provider)
      (setf *default-provider* provider))
  provider)

(defun get-provider (&optional domain)
  "Get provider for DOMAIN if present; otherwise default provider.
Example:
  (get-provider \"payments\")
  => #<PROVIDER ...>"
  (or (and domain (gethash domain *domain-providers*))
      *default-provider*))

(defun make-client (&key (name "client") domain evaluation-context)
  "Create a feature-flag client.
Example:
  (make-client :name \"checkout\" :domain \"payments\")
  => #S(FEATURE-FLAG-CLIENT ...)"
  (make-feature-flag-client :name name :domain domain
                            :evaluation-context (%ensure-context evaluation-context "client evaluation context")))

(defun create-client (&key (name "client") domain evaluation-context)
  "Create a feature-flag client (alias of MAKE-CLIENT).
Example:
  (create-client :name \"checkout\")
  => #S(FEATURE-FLAG-CLIENT ...)"
  (make-client :name name :domain domain :evaluation-context evaluation-context))

(defun %resolve-provider (client)
  (get-provider (feature-flag-client-domain client)))

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
                   (feature-flag-client-evaluation-context client)
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
  "Get boolean flag evaluation details.
Example:
  (get-boolean-details (create-client) \"beta\" nil)
  => #S(EVALUATION-DETAILS ...)"
  (%evaluate-details client :boolean flag-key default-value evaluation-context))

(defun get-string-details (client flag-key default-value &key evaluation-context)
  "Get string flag evaluation details.
Example:
  (get-string-details (create-client) \"variant\" \"control\")
  => #S(EVALUATION-DETAILS ...)"
  (%evaluate-details client :string flag-key default-value evaluation-context))

(defun get-number-details (client flag-key default-value &key evaluation-context)
  "Get number flag evaluation details.
Example:
  (get-number-details (create-client) \"max-items\" 10)
  => #S(EVALUATION-DETAILS ...)"
  (%evaluate-details client :number flag-key default-value evaluation-context))

(defun get-object-details (client flag-key default-value &key evaluation-context)
  "Get object flag evaluation details.
Example:
  (get-object-details (create-client) \"config\" '(:enabled nil))
  => #S(EVALUATION-DETAILS ...)"
  (%evaluate-details client :object flag-key default-value evaluation-context))

(defun get-boolean-value (client flag-key default-value &key evaluation-context)
  "Get boolean flag value.
Example:
  (get-boolean-value (create-client) \"beta\" nil)
  => T or NIL"
  (evaluation-details-value
   (get-boolean-details client flag-key default-value :evaluation-context evaluation-context)))

(defun get-string-value (client flag-key default-value &key evaluation-context)
  "Get string flag value.
Example:
  (get-string-value (create-client) \"variant\" \"control\")
  => \"control\" or provider-returned string"
  (evaluation-details-value
   (get-string-details client flag-key default-value :evaluation-context evaluation-context)))

(defun get-number-value (client flag-key default-value &key evaluation-context)
  "Get number flag value.
Example:
  (get-number-value (create-client) \"max-items\" 10)
  => 10 or provider-returned number"
  (evaluation-details-value
   (get-number-details client flag-key default-value :evaluation-context evaluation-context)))

(defun get-object-value (client flag-key default-value &key evaluation-context)
  "Get object flag value.
Example:
  (get-object-value (create-client) \"config\" '(:enabled nil))
  => (:enabled nil) or provider-returned object"
  (evaluation-details-value
   (get-object-details client flag-key default-value :evaluation-context evaluation-context)))
