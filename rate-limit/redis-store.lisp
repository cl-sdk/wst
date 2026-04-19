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
  ((key-prefix :initarg :key-prefix
               :initform "wst:rate-limit:"
               :reader redis-store-key-prefix)
   (window-seconds :initarg :window-seconds
                    :initform nil
                    :reader redis-store-window-seconds)
   (connection :initarg :connection
               :initform nil
               :reader redis-store-connection)
   (connection-fn :initarg :connection-fn
                  :initform (lambda (store thunk)
                              (let ((connection (redis-store-connection store)))
                                (unless connection
                                  (error "No Redis connection configured. Pass a CL-REDIS connection via :connection when creating the store."))
                                (let ((redis:*connection* connection))
                                  (funcall thunk))))
                  :reader redis-store-connection-fn))
  (:documentation "Redis-backed implementation of the rate-limit store protocol.

Slots:
- KEY-PREFIX: string prefix used to namespace Redis keys.
- WINDOW-SECONDS: optional TTL set on each saved key via EXPIRE.
- CONNECTION: CL-REDIS connection object used as REDIS:*CONNECTION*.
- CONNECTION-FN: function called as (connection-fn store thunk) to execute Redis calls."))

(defun redis-store--with-connection (store thunk)
  (funcall (redis-store-connection-fn store) store thunk))

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
         (reply (redis-store--with-connection store
                  (lambda () (redis:hmget redis-key "count" "start"))))
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
    (redis-store--with-connection store
      (lambda ()
        (redis:hmset redis-key
                     "count" (write-to-string count)
                     "start" (write-to-string start-time))))
    (when (and ttl (plusp ttl))
      (redis-store--with-connection store
        (lambda () (redis:expire redis-key ttl))))
    t))

(defmethod wst.rate-limit.store:delete-window ((store redis-store) key)
  (redis-store--with-connection store
    (lambda () (redis:del (redis-store--key store key)))))
