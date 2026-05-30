(defpackage #:io.github.cl-sdk.wst.authorization
  (:use #:cl)
  (:documentation "INCITS 359-inspired authorization primitives for wst.

Provides:
- RBAC-flavored entity classes for users, roles, permissions, and sessions
- Pluggable store protocol generics
- Lazy policy specifications composed from first-class policy functions
- Small DSL helpers for named policies and policy sets")
  (:export
   #:user
   #:user-id
   #:role
   #:role-name
   #:permission
   #:permission-operation
   #:permission-object
   #:session
   #:session-store
   #:assigned-roles
   #:role-permissions
   #:role-seniors
   #:create-session
   #:session-user
   #:active-roles
   #:activate-role
   #:deactivate-role
   #:session-authorized-p
   #:evaluate-policy
   #:defpolicy
   #:defpolicy-set
   #:require-authenticated
   #:require-role
   #:require-permission
   #:require-predicate
   #:all-of
   #:any-of
   #:invert
   #:deny-overrides
   #:permit-overrides
   #:first-applicable
   #:*decision-allow*
   #:*decision-deny*
   #:*decision-not-applicable*))

(in-package #:io.github.cl-sdk.wst.authorization)

(defparameter *decision-allow* :allow)
(defparameter *decision-deny* :deny)
(defparameter *decision-not-applicable* :not-applicable)

(defgeneric assigned-roles (store user)
  (:documentation "Return the roles assigned to USER in STORE.
Default entity-backed methods read roles from USER directly."))

(defgeneric role-permissions (store role)
  (:documentation "Return the permissions assigned to ROLE in STORE.
Default entity-backed methods read permissions from ROLE directly."))

(defgeneric role-seniors (store role)
  (:documentation "Return inherited roles for ROLE in STORE.
Default entity-backed methods read the hierarchy from ROLE directly."))

(defgeneric create-session (store user &key roles)
  (:documentation "Create a SESSION for USER in STORE.
ROLES, when provided, must be a subset of the roles assigned to USER."))

(defgeneric session-user (session)
  (:documentation "Return the user associated with SESSION."))

(defgeneric active-roles (session)
  (:documentation "Return the currently active roles for SESSION."))

(defgeneric activate-role (store session role)
  (:documentation "Activate ROLE for SESSION in STORE and return SESSION."))

(defgeneric deactivate-role (store session role)
  (:documentation "Deactivate ROLE for SESSION in STORE and return SESSION."))

(defgeneric session-authorized-p (store session operation object)
  (:documentation "Return true when SESSION is authorized for OPERATION on OBJECT."))

(defgeneric evaluate-policy (policy context)
  (:documentation "Evaluate POLICY against CONTEXT and return one of
`*decision-allow*`, `*decision-deny*`, or `*decision-not-applicable*`."))

(defclass user ()
  ((id :initarg :id :reader user-id :initform nil)
   (roles :initarg :roles :accessor %user-roles :initform nil))
  (:documentation "Default RBAC user entity."))

(defclass role ()
  ((name :initarg :name :reader role-name :initform (error "role.name is required."))
   (permissions :initarg :permissions :accessor %role-permissions :initform nil)
   (seniors :initarg :seniors :accessor %role-seniors :initform nil))
  (:documentation "Default RBAC role entity."))

(defclass permission ()
  ((operation :initarg :operation
              :reader permission-operation
              :initform (error "permission.operation is required."))
   (object :initarg :object
           :reader permission-object
           :initform (error "permission.object is required.")))
  (:documentation "Default RBAC permission entity."))

(defclass session ()
  ((user :initarg :user
         :accessor session-user
         :initform (error "session.user is required."))
   (store :initarg :store
          :reader session-store
          :initform nil)
   (active-roles-value :initarg :active-roles
                       :accessor active-roles
                       :initform nil))
  (:documentation "Default RBAC session entity."))

(defun %decision-p (value)
  (member value (list *decision-allow*
                      *decision-deny*
                      *decision-not-applicable*)
          :test #'eq))

(defun %ensure-decision (value)
  (unless (%decision-p value)
    (error "Invalid authorization decision: ~S" value))
  value)

(defun %role-key (role)
  (if (typep role 'role)
      (role-name role)
      role))

(defun %role= (left right)
  (equal (%role-key left) (%role-key right)))

(defun %permission-match-p (permission operation object)
  (typecase permission
    (permission
     (and (equal (permission-operation permission) operation)
          (equal (permission-object permission) object)))
    (cons
     (and (equal (car permission) operation)
          (equal (cdr permission) object)))
    (t nil)))

(defun %member-role-p (role roles)
  (member role roles :test #'%role=))

(defun %append-role (role roles)
  (if (%member-role-p role roles)
      roles
      (append roles (list role))))

(defun %reachable-roles (store roles)
  (labels ((walk (pending seen)
             (if (endp pending)
                 seen
                 (let* ((role (car pending))
                        (rest (cdr pending)))
                   (if (%member-role-p role seen)
                       (walk rest seen)
                       (walk (append rest (role-seniors store role))
                             (%append-role role seen)))))))
    (walk roles nil)))

(defun %session-from-context (context)
  (or (getf context :session)
      (let ((subject (getf context :subject)))
        (and (typep subject 'session)
             subject))))

(defun %store-from-context (context session)
  (or (getf context :store)
      (and session (session-store session))
      t))

(defmethod assigned-roles ((store t) (user user))
  (%user-roles user))

(defmethod role-permissions ((store t) (role role))
  (%role-permissions role))

(defmethod role-seniors ((store t) (role role))
  (%role-seniors role))

(defmethod create-session ((store t) (user user) &key roles)
  (let* ((assigned (assigned-roles store user))
         (selected (or roles assigned)))
    (unless (every (lambda (role) (%member-role-p role assigned)) selected)
      (error "Requested session roles must be a subset of the user's assigned roles."))
    (make-instance 'session
                   :user user
                   :store store
                   :active-roles (copy-list selected))))

(defmethod activate-role ((store t) (session session) role)
  (unless (%member-role-p role (assigned-roles store (session-user session)))
    (error "Cannot activate a role not assigned to the session user."))
  (setf (active-roles session)
        (%append-role role (active-roles session)))
  session)

(defmethod deactivate-role ((store t) (session session) role)
  (setf (active-roles session)
        (remove role (active-roles session) :test #'%role=))
  session)

(defmethod session-authorized-p ((store t) (session session) operation object)
  (let ((roles (%reachable-roles store (active-roles session))))
    (loop :for role :in roles
          :thereis (loop :for permission :in (role-permissions store role)
                         :thereis (%permission-match-p permission operation object)))))

(defmethod evaluate-policy ((policy function) context)
  (%ensure-decision (funcall policy context)))

(defun require-authenticated ()
  "Return a policy that allows only when CONTEXT carries a session."
  (lambda (context)
    (if (%session-from-context context)
        *decision-allow*
        *decision-deny*)))

(defun require-role (role)
  "Return a policy that allows only when ROLE is active in the session."
  (lambda (context)
    (let ((session (%session-from-context context)))
      (if (and session (%member-role-p role (active-roles session)))
          *decision-allow*
          *decision-deny*))))

(defun require-permission (operation object)
  "Return a policy that allows only when the session authorizes OPERATION on OBJECT."
  (lambda (context)
    (let ((session (%session-from-context context)))
      (if session
          (if (session-authorized-p (%store-from-context context session)
                                    session
                                    operation
                                    object)
              *decision-allow*
              *decision-deny*)
          *decision-deny*))))

(defun require-predicate (predicate)
  "Return a policy that delegates to PREDICATE.
Truthy values become `*decision-allow*`, false becomes `*decision-deny*`,
and explicit decisions are preserved."
  (lambda (context)
    (let ((value (funcall predicate context)))
      (if (%decision-p value)
          value
          (if value
              *decision-allow*
              *decision-deny*)))))

(defun all-of (&rest policies)
  "Return a policy that allows only when all POLICIES allow."
  (lambda (context)
    (let ((saw-not-applicable nil))
      (dolist (policy policies (if saw-not-applicable
                                   *decision-not-applicable*
                                   *decision-allow*))
        (case (evaluate-policy policy context)
          (:deny (return *decision-deny*))
          (:not-applicable (setf saw-not-applicable t)))))))

(defun any-of (&rest policies)
  "Return a policy that allows when any POLICY allows."
  (lambda (context)
    (let ((saw-deny nil))
      (dolist (policy policies (if saw-deny
                                   *decision-deny*
                                   *decision-not-applicable*))
        (case (evaluate-policy policy context)
          (:allow (return *decision-allow*))
          (:deny (setf saw-deny t)))))))

(defun invert (policy)
  "Return a policy with allow and deny decisions inverted."
  (lambda (context)
    (case (evaluate-policy policy context)
      (:allow *decision-deny*)
      (:deny *decision-allow*)
      (t *decision-not-applicable*))))

(defun deny-overrides (&rest policies)
  "Return a policy-set where any deny decision wins."
  (lambda (context)
    (let ((saw-allow nil))
      (dolist (policy policies (if saw-allow
                                   *decision-allow*
                                   *decision-not-applicable*))
        (case (evaluate-policy policy context)
          (:deny (return *decision-deny*))
          (:allow (setf saw-allow t)))))))

(defun permit-overrides (&rest policies)
  "Return a policy-set where any allow decision wins."
  (lambda (context)
    (let ((saw-deny nil))
      (dolist (policy policies (if saw-deny
                                   *decision-deny*
                                   *decision-not-applicable*))
        (case (evaluate-policy policy context)
          (:allow (return *decision-allow*))
          (:deny (setf saw-deny t)))))))

(defun first-applicable (&rest policies)
  "Return a policy-set that chooses the first non-not-applicable decision."
  (lambda (context)
    (dolist (policy policies *decision-not-applicable*)
      (let ((decision (evaluate-policy policy context)))
        (unless (eq decision *decision-not-applicable*)
          (return decision))))))

(defmacro defpolicy (name form)
  `(defparameter ,name ,form))

(defmacro defpolicy-set (name &key combiner policies)
  `(defparameter ,name (apply #',combiner ,policies)))
