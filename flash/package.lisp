(defpackage #:io.github.cl-sdk.wst.flash
  (:use #:cl)
  (:export
   #:flash-messages
   #:append-flash-message
   #:consume-flash-messages
   #:clear-flash-messages
   #:valid-flash-type-p))

(in-package #:io.github.cl-sdk.wst.flash)

(defgeneric flash-messages (session &key &allow-other-keys)
  (:documentation "Return queued flash messages for SESSION-ID without consuming them.

Always returns a list (possibly empty)."))

(defgeneric append-flash-message (session text &key &allow-other-keys)
  (:documentation "Append a normalized flash message for SESSION and return that message.

Signals an error for invalid payload/type or queue overflow."))

(defgeneric consume-flash-messages (session &key &allow-other-keys)
  (:documentation "Return and clear queued flash messages for SESSION.

Always returns a list (possibly empty)."))

(defgeneric clear-flash-messages (session &key &allow-other-keys)
  (:documentation "Clear queued flash messages for SESSION.

Returns T when a queue was removed, NIL otherwise."))
