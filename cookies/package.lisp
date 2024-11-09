(defpackage #:wst.cookies
  (:use #:cl)
  (:export
   #:initialize-session
   #:recover-session
   #:terminate-session
   #:update-session))

(in-package :wst.cookies)

(defgeneric initialize-session (driver data &key &allow-other-keys))
(defgeneric recover-session (driver session-id &key &allow-other-keys))
(defgeneric terminate-session (driver session &key &allow-other-keys))
(defgeneric update-session (driver session &key &allow-other-keys))
