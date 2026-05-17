(defpackage #:io.github.cl-sdk.wst.feature-flag
  (:use #:cl)
  (:documentation "Feature-flag primitives for wst.

MVP implemented:
- User-defined provider resolution via object-of-interest + domain
- Client creation and domain-aware provider lookup
- Evaluation context merge (client -> invocation)
- Typed evaluations: boolean/string/number/object
- Evaluation details with reason and error metadata

Planned for later phases:
- Hooks pipeline
- Provider events / status
- Extended lifecycle and transaction/request propagation")
  (:export
   #:provider
   #:provider-name
   #:provider-metadata
   #:initialize-provider
   #:shutdown-provider
   #:client
   #:client-name
   #:resolve-boolean-details
   #:resolve-string-details
   #:resolve-number-details
   #:resolve-object-details
   #:acquire-client
   #:resolve-provider
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

(defgeneric resolve-provider (object domain)
  (:documentation "Resolve provider for OBJECT and DOMAIN.
Users should implement this generic to select the correct provider from app/request state.
Default method fallback returns a noop provider when no specialized method exists.
Example:
  (resolve-provider request \"payments\")
  => #<PROVIDER ...>"))

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

(defgeneric acquire-client (provider &key domain)
  (:documentation "Return a client from a PROVIDER. Users are responsible
for what `acquire` means."))

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

(defclass provider ()
  ((name :initarg :name :accessor provider-name :initform (error "provider.name is required.")))
  (:documentation "Base provider protocol class."))

(defclass client ()
  ((provider :type provider
             :initarg :provider
             :accessor client-provider
             :initform (error "client.provider is required."))
   (domain :type string
           :initarg :domain
           :accessor client-domain
           :initform (error "client.domain is required."))
   (evaluation-context :type list
                       :initarg :client-evaluation-context
                       :initform nil))
  (:documentation "Base provider's client class."))

(defstruct evaluation-details
  "Evaluation detail record for feature-flag metadata."
  flag-key
  value
  variant
  reason
  error-code
  error-message
  metadata)

(defun %plist-even-p (plist)
  (and (listp plist)
       (evenp (length plist))))

(defun %ensure-context (context where)
  (unless (%plist-even-p context)
    (error "~A must be a plist (even-length list)." where))
  context)

(defun merge-evaluation-contexts (&rest contexts)
  "Merge context plists in argument order so later contexts override earlier keys.
Only explicit contexts passed by caller are merged (no implicit API/global context).
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

(defmethod provider-metadata ((provider provider))
  (list :name (provider-name provider)))

(defmethod initialize-provider ((provider provider))
  provider)

(defmethod shutdown-provider ((provider provider))
  provider)

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
  (let* ((provider (client-provider client))
         (context (merge-evaluation-contexts
                   (slot-value client 'evaluation-context)
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
  (get-boolean-details (acquire-client provider) \"beta\" nil)
  => #S(EVALUATION-DETAILS ...)"
  (%evaluate-details client :boolean flag-key default-value evaluation-context))

(defun get-string-details (client flag-key default-value &key evaluation-context)
  "Get string flag evaluation details.
Example:
  (get-string-details (acquire-client provider) \"variant\" \"control\")
  => #S(EVALUATION-DETAILS ...)"
  (%evaluate-details client :string flag-key default-value evaluation-context))

(defun get-number-details (client flag-key default-value &key evaluation-context)
  "Get number flag evaluation details.
Example:
  (get-number-details (acquire-client provider) \"max-items\" 10)
  => #S(EVALUATION-DETAILS ...)"
  (%evaluate-details client :number flag-key default-value evaluation-context))

(defun get-object-details (client flag-key default-value &key evaluation-context)
  "Get object flag evaluation details.
Example:
  (get-object-details (acquire-client provider) \"config\" '(:enabled nil))
  => #S(EVALUATION-DETAILS ...)"
  (%evaluate-details client :object flag-key default-value evaluation-context))

(defun get-boolean-value (client flag-key default-value &key evaluation-context)
  "Get boolean flag value.
Example:
  (get-boolean-value (acquire-client provider) \"beta\" nil)
  => T or NIL"
  (evaluation-details-value
   (get-boolean-details client flag-key default-value :evaluation-context evaluation-context)))

(defun get-string-value (client flag-key default-value &key evaluation-context)
  "Get string flag value.
Example:
  (get-string-value (acquire-client provider) \"variant\" \"control\")
  => \"control\" or provider-returned string"
  (evaluation-details-value
   (get-string-details client flag-key default-value :evaluation-context evaluation-context)))

(defun get-number-value (client flag-key default-value &key evaluation-context)
  "Get number flag value.
Example:
  (get-number-value (acquire-client provider) \"max-items\" 10)
  => 10 or provider-returned number"
  (evaluation-details-value
   (get-number-details client flag-key default-value :evaluation-context evaluation-context)))

(defun get-object-value (client flag-key default-value &key evaluation-context)
  "Get object flag value.
Example:
  (get-object-value (acquire-client provider) \"config\" '(:enabled nil))
  => (:enabled nil) or provider-returned object"
  (evaluation-details-value
   (get-object-details client flag-key default-value :evaluation-context evaluation-context)))
