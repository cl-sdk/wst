(defpackage #:io.github.cl-sdk.wst.session.sqlite
  (:use #:cl)
  (:import-from #:io.github.cl-sdk.wst.session
                #:create-session
                #:recover-session
                #:update-session
                #:session-exists-p
                #:renew-session
                #:terminate-session)
  (:export
   #:sqlite-store
   #:sqlite-store-connection
   #:sqlite-store-database-path
   #:sqlite-store-table-name
   #:sqlite-store-max-age-seconds
   #:initialize-sqlite-store))

(in-package #:io.github.cl-sdk.wst.session.sqlite)

(declaim (inline now))
(defun now ()
  (get-universal-time))

;; queries

(declaim (inline delete-expired-session-statement))
(defun delete-expired-session-statement (table-name)
  (format nil "DELETE FROM ~a WHERE expires_at <= ?" table-name))

(declaim (inline find-session-by-id-statement))
(defun find-session-by-id-statement (table-name)
    (format nil "SELECT id, payload, created_at, updated_at, expires_at
                       FROM ~a
                       WHERE id = ?
                       LIMIT 1"
            table-name))

(declaim (inline create-table-statement))
(defun create-table-statement (table-name)
    (format nil "CREATE TABLE IF NOT EXISTS ~a (
                      id TEXT PRIMARY KEY,
                      payload TEXT NOT NULL,
                      created_at INTEGER NOT NULL,
                      updated_at INTEGER NOT NULL,
                      expires_at INTEGER NOT NULL
                    )"
            table-name))

(declaim (inline create-expires-at-index-statement))
(defun create-expires-at-index-statement (table-name)
  (format nil "CREATE INDEX IF NOT EXISTS idx_~a_expires_at
                    ON ~a(expires_at)"
          table-name
          table-name))

(declaim (inline insert-session-statement))
(defun insert-session-statement (table-name)
    (format nil "INSERT INTO ~a (id, payload, created_at, updated_at, expires_at)
 VALUES (?, ?, ?, ?, ?) returning id, payload, created_at, updated_at, expires_at"
            table-name))

(declaim (inline update-session-statement))
(defun update-session-statement (table-name)
    (format nil "UPDATE ~a SET payload = ?, updated_at = ?, expires_at = ? WHERE id = ? RETURNING id, payload, created_at, updated_at, expires_at"
            table-name))

(declaim (inline renew-session-statement))
(defun renew-session-statement (table-name)
  (format nil "UPDATE ~a SET expires_at = ?, updated_at = ? WHERE id = ? RETURNING id, payload, created_at, updated_at, expires_at"
          table-name))

(declaim (inline delete-session-statement))
(defun delete-session-statement (table-name)
  (format nil "DELETE FROM ~a WHERE id = ?" table-name))

;; storage class

(defclass sqlite-store ()
  ((connection :initarg :connection
               :accessor sqlite-store-connection)
   (database-path :initarg :database-path
                  :initform #P"sessions.sqlite3"
                           :accessor sqlite-store-database-path)
   (table-name :initarg :table-name
               :initform "sessions"
               :accessor sqlite-store-table-name)
   (max-age-seconds :initarg :max-age-seconds
                    :initform 3600
                    :accessor sqlite-store-max-age-seconds)
   (data-serializer :initarg :data-serializer
                    :accessor sqlite-store-data-serializer)
   (data-deserializer :initarg :data-deserializer
                      :accessor sqlite-store-data-deserializer)
   (database-lock :initarg :database-lock
                  :reader sqlite-store-lock
                  :initform
                  #+sbcl
                  (sb-thread:make-mutex :name "sqlite-store-lock")
                  #-sbcl
                  nil)))

(defmacro with-store-lock ((store) &body body)
  #+sbcl
  `(sb-thread:with-mutex ((sqlite-store-lock ,store))
     ,@body)
  #-sbcl
  `(progn ,@body))

(declaim (inline %row-column))
(defun %row-column (row index)
  (etypecase row
    (list (nth index row))
    (vector (aref row index))))

(defun cleanup-expired-sessions (store date)
  (sqlite:execute-non-query (sqlite-store-connection store)
                            (delete-expired-session-statement (store-table-name store))
                            date))

(defun initialize-sqlite-store (store)
  "Initializes SQLite schema objects for STORE.

 Call this once during server startup, before using STORE for session operations."
  (check-type store sqlite-store)
  (with-store-lock (store)
    (let ((table-name (store-table-name store)))
      (sqlite:execute-non-query
       (sqlite-store-connection store)
       (create-table-statement table-name))
      (sqlite:execute-non-query
       (sqlite-store-connection store)
       (create-expires-at-index-statement table-name)))))

(defun %make-session-object (session-id data created-at updated-at expires-at)
  (list :id session-id
        :data data
        :created-at created-at
        :updated-at updated-at
        :expires-at expires-at))

(defun %find-session (store session-id)
  (car (sqlite:execute-to-list
        (sqlite-store-connection store)
        (find-session-by-id-statement (store-table-name store))
        session-id)))

(defun %session-object-from-row (store row)
  (make-session-object (row-column row 0)
                       (funcall (sqlite-store-data-deserializer store) (row-column row 1))
                       (row-column row 2)
                       (row-column row 3)
                       (row-column row 4)))

(defun %recover-session (store session-id)
  (let ((row (%find-session store session-id)))
    (when row
      (%session-object-from-row store row))))

(defmethod io.github.cl-sdk.wst.session:create-session ((store sqlite-store) data &key session-id ttl-seconds &allow-other-keys)
  (with-store-lock (store)
    (let* ((created-at (now))
           (expires-at (+ created-at
                          (or ttl-seconds
                             (sqlite-store-max-age-seconds store)))))
      (let ((row (car (sqlite:execute-to-list
                       (sqlite-store-connection store)
                       (insert-session-statement (store-table-name store))
                       session-id
                       (funcall (sqlite-store-data-serializer store) data)
                       created-at
                       created-at
                       expires-at))))
        (%session-object-from-row store row)))))

(defmethod io.github.cl-sdk.wst.session:recover-session ((store sqlite-store) session-id &key &allow-other-keys)
  (with-store-lock (store)
    (cleanup-expired-sessions store)
    (%recover-session store session-id)))

(defmethod io.github.cl-sdk.wst.session:update-session ((store sqlite-store) session &key &allow-other-keys)
  (with-store-lock (store)
    (cleanup-expired-sessions store)
    (let* ((id (getf session :id))
           (data (getf session :data))
           (updated-at (now))
           (expires-at (or (getf session :expires-at)
                          (+ updated-at (sqlite-store-max-age-seconds store)))))
      (unless id
        (error "Session object must include :id when updating."))
      (let ((row (car (sqlite:execute-to-list
                       (sqlite-store-connection store)
                       (update-session-statement (store-table-name store))
                       (funcall (sqlite-store-data-serializer store) data)
                       updated-at
                       expires-at
                       id))))
        (%session-object-from-row store row)))))

(defmethod io.github.cl-sdk.wst.session:session-exists-p ((store sqlite-store) session-id &key &allow-other-keys)
  (with-store-lock (store)
    (cleanup-expired-sessions store)
    (not (null (%find-session store session-id)))))

(defmethod io.github.cl-sdk.wst.session:renew-session ((store sqlite-store) session-id &optional additional-time &key &allow-other-keys)
  (with-store-lock (store)
    (cleanup-expired-sessions store)
    (let* ((row (%find-session store session-id)))
      (when row
        (let* ((new-expires-at (+ (now)
                                  (or additional-time
                                     (sqlite-store-max-age-seconds store))))
               (updated-row (car (sqlite:execute-to-list
                                  (sqlite-store-connection store)
                                  (renew-session-statement (store-table-name store))
                                  new-expires-at
                                  (now)
                                  session-id))))
          (%session-object-from-row store updated-row))
        t))))

(defmethod io.github.cl-sdk.wst.session:terminate-session ((store sqlite-store) session-id &key &allow-other-keys)
  (with-store-lock (store)
    (sqlite:execute-non-query
     (sqlite-store-connection store)
     (delete-session-statement (store-table-name store))
     session-id)))
