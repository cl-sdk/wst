(defpackage :io.github.cl-sdk.wst.rate-limit.redis-store
  (:use #:cl)
  (:documentation "Redis-backed storage backend for wst.rate-limit.

This backend implements the wst.rate-limit.store protocol using Redis hash
entries with fields:
  - \"count\" : current request count
  - \"start\" : window start time (universal-time)

Redis access is delegated to COMMAND-FN, allowing integration with any Redis
client library. COMMAND-FN receives command name as a string followed by
command arguments.")
  (:export
   #:redis-store))

(in-package :io.github.cl-sdk.wst.rate-limit.redis-store)

(defclass redis-store ()
  ((command-fn :initarg :command-fn
               :initform (lambda (&rest _)
                           (declare (ignore _))
                           (error "No Redis command function configured. Pass :COMMAND-FN when creating REDIS-STORE."))
               :reader redis-store-command-fn)
   (key-prefix :initarg :key-prefix
               :initform "wst:rate-limit:"
               :reader redis-store-key-prefix)
   (window-seconds :initarg :window-seconds
                   :initform nil
                   :reader redis-store-window-seconds))
  (:documentation "Redis-backed implementation of the rate-limit store protocol.

Slots:
- COMMAND-FN: function called as (command-fn command &rest args).
- KEY-PREFIX: string prefix used to namespace Redis keys.
- WINDOW-SECONDS: optional TTL set on each saved key via EXPIRE."))

(defun redis-store--call (store command &rest args)
  (apply (redis-store-command-fn store) command args))

(defun redis-store--key (store key)
  (format nil "~a~a" (redis-store-key-prefix store) (write-to-string key :readably t)))

(defun redis-store--integer-or-nil (value)
  (cond
    ((null value) nil)
    ((integerp value) value)
    ((stringp value)
     (handler-case (parse-integer value :junk-allowed nil)
       (error () nil)))
    (t nil)))

(defmethod io.github.cl-sdk.wst.rate-limit.store:fetch-window ((store redis-store) key)
  (let* ((redis-key (redis-store--key store key))
         (reply (redis-store--call store "HMGET" redis-key "count" "start"))
         (count-raw (and (listp reply) (first reply)))
         (start-raw (and (listp reply) (second reply)))
         (count (redis-store--integer-or-nil count-raw))
         (start (redis-store--integer-or-nil start-raw)))
    (if (and count start)
        (values count start)
        (values nil nil))))

(defmethod io.github.cl-sdk.wst.rate-limit.store:save-window ((store redis-store) key count start-time)
  (let* ((redis-key (redis-store--key store key))
         (ttl (redis-store-window-seconds store)))
    (redis-store--call store "HSET" redis-key
                       "count" (write-to-string count)
                       "start" (write-to-string start-time))
    (when (and ttl (plusp ttl))
      (redis-store--call store "EXPIRE" redis-key ttl))
    t))

(defmethod io.github.cl-sdk.wst.rate-limit.store:delete-window ((store redis-store) key)
  (redis-store--call store "DEL" (redis-store--key store key)))
