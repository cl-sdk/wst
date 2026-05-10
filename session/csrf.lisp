(defpackage #:io.github.cl-sdk.wst.session.csrf
  (:use #:cl)
  (:export
   #:session-csrf-token
   #:add-session-csrf-token
   #:remove-session-csrf-token
   #:verify-session-csrf-token))

(in-package :io.github.cl-sdk.wst.session.csrf)

(defgeneric session-csrf-token (obj &key &allow-other-keys))
(defgeneric add-session-csrf-token (obj key &key &allow-other-keys))
(defgeneric remove-session-csrf-token (obj &key &allow-other-keys))
(defgeneric verify-session-csrf-token (obj key &key &allow-other-keys))
