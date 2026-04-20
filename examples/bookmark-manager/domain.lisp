(in-package #:wst.example.bookmark-manager)

(defparameter *database-path*
  (merge-pathnames #P"bookmarks.sqlite3" *load-truename*))
(defparameter *database-connection* nil)
(defparameter *session-max-age-seconds* 3600)

(defparameter *sessions* (make-hash-table :test 'equal))

#+sbcl
(defparameter *sessions-lock*
  (sb-thread:make-mutex :name "bookmark-manager-sessions"))
#+sbcl
(defparameter *database-lock*
  (sb-thread:make-mutex :name "bookmark-manager-database"))

(defmacro with-sessions-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*sessions-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defmacro with-database-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*database-lock*)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun make-session-id ()
  #+sbcl
  (handler-case
      (let ((bytes (make-array 32 :element-type '(unsigned-byte 8))))
        (with-open-file (in "/dev/urandom"
                            :direction :input
                            :element-type '(unsigned-byte 8))
          (let ((read-count (read-sequence bytes in)))
            (unless (= read-count (length bytes))
              (error "Insufficient random bytes read from /dev/urandom."))))
        (bytes-to-hex-string bytes))
    (error ()
      (error "Unable to read secure random bytes for session id generation.")))
  #-sbcl
  (error "Secure session id generation currently requires SBCL."))

(defun current-timestamp ()
  (get-universal-time))

(defun sweep-expired-sessions ()
  (let ((now (current-timestamp)))
    (let ((expired-session-ids nil))
      (maphash (lambda (session-id entry)
                 (when (<= (getf entry :expires-at 0) now)
                   (push session-id expired-session-ids)))
               *sessions*)
      (dolist (session-id expired-session-ids)
        (remhash session-id *sessions*)))))

(defun create-session (user)
  (with-sessions-lock
    (sweep-expired-sessions)
    (loop for session-id = (make-session-id)
          unless (gethash session-id *sessions*)
            do (setf (gethash session-id *sessions*)
                     (list :user user
                           :expires-at (+ (current-timestamp) *session-max-age-seconds*)))
               (return session-id))))

(defun destroy-session (session-id)
  (with-sessions-lock
    (remhash session-id *sessions*)))

(defun session-user (session-id)
  (with-sessions-lock
    (let ((entry (gethash session-id *sessions*)))
      (cond
        ((null entry) nil)
        ((<= (getf entry :expires-at 0) (current-timestamp))
         (remhash session-id *sessions*)
         nil)
        (t (getf entry :user))))))

(defun ensure-database-connection ()
  (unless *database-connection*
    (setf *database-connection* (sqlite:connect *database-path*))
    (sqlite:execute-non-query *database-connection*
                              "CREATE TABLE IF NOT EXISTS bookmarks (
                                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                                 user TEXT NOT NULL,
                                 title TEXT NOT NULL,
                                 url TEXT NOT NULL,
                                 created_at TEXT DEFAULT CURRENT_TIMESTAMP
                               )")
    (sqlite:execute-non-query *database-connection*
                              "CREATE INDEX IF NOT EXISTS idx_bookmarks_user
                               ON bookmarks(user)")))

(defun bookmarks-for-user (user)
  (with-database-lock
    (sqlite:execute-to-list *database-connection*
                            "SELECT id, title, url
                             FROM bookmarks
                             WHERE user = ?
                             ORDER BY id DESC"
                            user)))

(defun add-bookmark (user title url)
  (with-database-lock
    (sqlite:execute-non-query *database-connection*
                              "INSERT INTO bookmarks(user, title, url)
                               VALUES (?, ?, ?)"
                              user title url)))

(defun bookmark-exists-for-user-p (user bookmark-id)
  (with-database-lock
    (not (null (sqlite:execute-single *database-connection*
                                      "SELECT id FROM bookmarks
                                       WHERE id = ? AND user = ?
                                       LIMIT 1"
                                      bookmark-id user)))))

(defun delete-bookmark (user bookmark-id)
  (with-database-lock
    (sqlite:execute-non-query *database-connection*
                              "DELETE FROM bookmarks WHERE id = ? AND user = ?"
                              bookmark-id user)))
