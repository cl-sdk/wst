(defpackage #:wst.session.http
  (:nicknames #:wst.cookies)
  (:use #:cl)
  (:export
   #:initialize-session
   #:recover-session
   #:terminate-session
   #:update-session))

(in-package :wst.session.http)

(defgeneric initialize-session (driver data &key &allow-other-keys)
  (:documentation "Initializes a new session using the given DRIVER and initial DATA.

- DRIVER: The session backend or mechanism.
- DATA: Initial data for the session.
- Additional keyword arguments may be accepted by specific implementations.

Returns a new session object."))

(defgeneric recover-session (driver session-id &key &allow-other-keys)
  (:documentation "Recovers an existing session by SESSION-ID using the specified DRIVER.

- DRIVER: The session backend or mechanism.
- SESSION-ID: The identifier of the session to recover.
- Additional keyword arguments may be accepted by specific implementations.

Returns the recovered session object or NIL if not found."))

(defgeneric terminate-session (driver session &key &allow-other-keys)
  (:documentation "Terminates the given SESSION using the specified DRIVER.

- DRIVER: The session backend or mechanism.
- SESSION: The session object to terminate.
- Additional keyword arguments may be accepted by specific implementations.

Performs cleanup or removal of the session."))

(defgeneric update-session (driver session &key &allow-other-keys)
  (:documentation "Updates the given SESSION with new state or data using the specified DRIVER.

- DRIVER: The session backend or mechanism.
- SESSION: The session object to update.
- Additional keyword arguments may be accepted by specific implementations.

Persists changes to the session."))
