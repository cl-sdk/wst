(defpackage #:wst.session
  (:use #:cl)
  (:export
   #:create-session
   #:recover-session
   #:update-session
   #:session-exists-p
   #:renew-session
   #:terminate-session))

(in-package :wst.session)

(defgeneric create-session (object data &key &allow-other-keys)
  (:documentation "Creates a new session for a user, returning a unique session ID."))

(defgeneric recover-session (object session-id &key &allow-other-keys)
  (:documentation "Retrieves the session data for the given session ID, or NIL if expired or non-existent."))

(defgeneric update-session (object session &key &allow-other-keys)
  (:documentation "Updates the SESSION for the given session ID."))

(defgeneric session-exists-p (object session-id &key &allow-other-keys)
  (:documentation "Returns T if the session exists and is active, NIL otherwise."))

(defgeneric renew-session (object session-id &optional additional-time &key &allow-other-keys)
  (:documentation "Renews the session by extending its expiry time."))

(defgeneric terminate-session (object session-id &key &allow-other-keys)
  (:documentation "Destroys the session with the given session ID."))
