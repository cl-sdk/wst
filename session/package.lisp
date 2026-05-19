(defpackage #:io.github.cl-sdk.wst.session
  (:use #:cl)
  (:export
   #:create-session
   #:access-session
   #:update-session
   #:session-exists-p
   #:renew-session
   #:terminate-session
   #:cleanup-expired-sessions))

(in-package :io.github.cl-sdk.wst.session)

(defgeneric create-session (object data &key &allow-other-keys)
  (:documentation "Creates a new session in OBJECT using the provided DATA.

- OBJECT: The session backend or manager.
- DATA: Initial data to associate with the new session.
- Additional keyword arguments may be accepted by specific implementations.

Returns the newly created session object."))

(defgeneric access-session (object session-id &key &allow-other-keys)
  (:documentation "Retrieves the session data associated with SESSION-ID from OBJECT and
it also must update the `last access at` if the session is valid.

This method must return this values:

- (:not-found session-id)
- (:expired session-id)
- (:session session)

- OBJECT: The session backend or manager.
- SESSION-ID: The identifier of the session to recover.

Additional keyword arguments may be accepted by specific implementations."))

(defgeneric update-session (object session &key &allow-other-keys)
  (:documentation "Persists updates to the given SESSION in OBJECT.

- OBJECT: The session backend or manager.
- SESSION: The session object to update.

Additional keyword arguments may be accepted by specific implementations."))

(defgeneric session-exists-p (object session-id &key &allow-other-keys)
  (:documentation "Checks whether a session identified by SESSION-ID exists and is active in OBJECT.

Returns T if the session exists and is active; NIL otherwise.

- OBJECT: The session backend or manager.
- SESSION-ID: The identifier of the session to check.

Additional keyword arguments may be accepted by specific implementations."))

(defgeneric renew-session (object session-id &optional additional-time &key &allow-other-keys)
  (:documentation "Extends the expiry time of the session identified by SESSION-ID in OBJECT.

- OBJECT: The session backend or manager.
- SESSION-ID: The identifier of the session to renew.
- ADDITIONAL-TIME (optional): Duration to extend the session’s expiry time.

Additional keyword arguments may be accepted by specific implementations."))

(defgeneric terminate-session (object session-id &key &allow-other-keys)
  (:documentation "Terminates and removes the session identified by SESSION-ID from OBJECT.

- OBJECT: The session backend or manager.
- SESSION-ID: The identifier of the session to terminate.

Additional keyword arguments may be accepted by specific implementations."))

(defgeneric cleanup-expired-sessions (object &key &allow-other-keys)
  (:documentation "Maintainance method to clean-up expired sessions.

- OBJECT: The session backend or manager.

Additional keyword arguments may be accepted by specific implementations."))
