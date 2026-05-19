(defpackage :io.github.cl-sdk.wst.session.sqlite.test
  (:use :cl :fiveam :io.github.cl-sdk.wst.session.sqlite))

(in-package :io.github.cl-sdk.wst.session.sqlite.test)

(5am:def-suite io.github.cl-sdk.wst.session.sqlite-store.suite
  :description "Tests for SQLite-backed io.github.cl-sdk.wst.session store.")

(5am:in-suite io.github.cl-sdk.wst.session.sqlite-store.suite)

(defun temporary-sqlite-database-path ()
  (make-pathname :directory '(:absolute "tmp")
                 :name (format nil "io.github.cl-sdk.wst.session-store-test-~a" (gensym))
                 :type "sqlite3"))

(defun serialize-session-data (data)
  (let ((*print-readably* t)
        (*print-circle* t))
    (write-to-string data)))

(defun deserialize-session-data (payload)
  (let ((*read-eval* nil))
    (read-from-string payload)))

(defmacro with-sqlite-session-store ((store-var) &body body)
  `(let* ((db-path (temporary-sqlite-database-path))
          (,store-var (make-instance 'io.github.cl-sdk.wst.session.sqlite:sqlite-store
                                     :connection (sqlite:connect db-path)
                                     :database-path db-path
                                     :data-serializer #'serialize-session-data
                                     :data-deserializer #'deserialize-session-data
                                     :max-age-seconds 300)))
     (unwind-protect
          (progn
            (io.github.cl-sdk.wst.session.sqlite:initialize-sqlite-store ,store-var)
            ,@body)
       (when (probe-file db-path)
         (delete-file db-path)))))

(5am:def-test sqlite-store-initialize-is-idempotent ()
  (with-sqlite-session-store (store)
    (5am:finishes
      (io.github.cl-sdk.wst.session.sqlite:initialize-sqlite-store store))
    (let* ((created (io.github.cl-sdk.wst.session:create-session store
                                                                 '(:user "alice")
                                                                 :session-id "idempotent-init-session"))
           (recovered (io.github.cl-sdk.wst.session:access-session store (getf created :id))))
      (5am:is (equal "idempotent-init-session" (getf recovered :id)))
      (5am:is (equal '(:user "alice") (getf recovered :data))))))

(5am:def-test sqlite-store-initialize-requires-sqlite-store ()
  (5am:signals type-error
    (io.github.cl-sdk.wst.session.sqlite:initialize-sqlite-store nil)))

(5am:def-test sqlite-store-create-and-recover-session ()
  (with-sqlite-session-store (store)
    (let* ((created (io.github.cl-sdk.wst.session:create-session store
                                                                 '(:user "alice")
                                                                 :session-id "session-id"))
           (session-id (getf created :id))
           (recovered (io.github.cl-sdk.wst.session:access-session store session-id)))
      (5am:is-true session-id)
      (5am:is (equal '(:user "alice") (getf recovered :data)))
      (5am:is (equal session-id (getf recovered :id)))
      (5am:is (integerp (getf recovered :last-accessed-at))))))

(5am:def-test sqlite-store-access-session-updates-last-accessed-at ()
  (with-sqlite-session-store (store)
    (let* ((created (io.github.cl-sdk.wst.session:create-session store
                                                                 '(:user "alice")
                                                                 :session-id "session-id"))
           (session-id (getf created :id)))
      (sqlite:execute-non-query (io.github.cl-sdk.wst.session.sqlite:sqlite-store-connection store)
                                (format nil "UPDATE ~a SET updated_at = 0, last_accessed_at = 0 WHERE id = ?"
                                        (io.github.cl-sdk.wst.session.sqlite:sqlite-store-table-name store))
                                session-id)
      (let ((accessed (io.github.cl-sdk.wst.session:access-session store session-id)))
        (5am:is (equal session-id (getf accessed :id)))
        (5am:is (equal '(:user "alice") (getf accessed :data)))
        (5am:is (> (getf accessed :updated-at) 0))
        (5am:is (> (getf accessed :last-accessed-at) 0))
        (5am:is (= (getf accessed :updated-at) (getf accessed :last-accessed-at)))))))

(5am:def-test sqlite-store-update-session ()
  (with-sqlite-session-store (store)
    (let* ((created (io.github.cl-sdk.wst.session:create-session store
                                                                 '(:user "alice")
                                                                 :session-id "session-id"))
           (updated (copy-list created)))
      (setf (getf updated :data) '(:user "bob"))
      (io.github.cl-sdk.wst.session:update-session store updated)
      (let ((recovered (io.github.cl-sdk.wst.session:access-session store (getf created :id))))
        (5am:is (equal '(:user "bob") (getf recovered :data)))))))

(5am:def-test sqlite-store-renew-session ()
  (with-sqlite-session-store (store)
    (let* ((created (io.github.cl-sdk.wst.session:create-session store '(:user "alice")
                                                                 :session-id "session-id"
                                                                 :ttl-seconds 10))
           (session-id (getf created :id))
           (before (getf (io.github.cl-sdk.wst.session:access-session store session-id) :expires-at)))
      (io.github.cl-sdk.wst.session:renew-session store session-id 120)
      (let ((after (getf (io.github.cl-sdk.wst.session:access-session store session-id) :expires-at)))
        (5am:is (> after before))))))

(5am:def-test sqlite-store-terminates-session ()
  (with-sqlite-session-store (store)
    (let* ((created (io.github.cl-sdk.wst.session:create-session store
                                                                 '(:user "alice")
                                                                 :session-id "session-id"))
           (session-id (getf created :id)))
      (5am:is-true (io.github.cl-sdk.wst.session:session-exists-p store session-id))
      (io.github.cl-sdk.wst.session:terminate-session store session-id)
      (5am:is-false (io.github.cl-sdk.wst.session:session-exists-p store session-id))
      (5am:is-false (io.github.cl-sdk.wst.session:access-session store session-id)))))

(5am:def-test sqlite-store-does-not-recover-expired-session ()
  (with-sqlite-session-store (store)
    (let* ((created (io.github.cl-sdk.wst.session:create-session
                     store
                     '(:user "alice")
                     :session-id "session-id"
                     :ttl-seconds 0))
           (session-id (getf created :id)))
      (5am:is-false (io.github.cl-sdk.wst.session:access-session store session-id))
      (5am:is-false (io.github.cl-sdk.wst.session:session-exists-p store session-id)))))

(5am:def-test sqlite-store-session-state-transitions ()
  (with-sqlite-session-store (store)
    (let* ((active-ttl-seconds 60)
           (renewal-ttl-seconds 120)
           (created (io.github.cl-sdk.wst.session:create-session
                     store
                     '(:user "alice")
                     :session-id "active-session"
                     :ttl-seconds active-ttl-seconds))
           (session-id (getf created :id))
           (active (io.github.cl-sdk.wst.session:access-session store session-id)))
      (5am:is-true (io.github.cl-sdk.wst.session:session-exists-p store session-id))
      (5am:is-true active)
      (let ((before (getf active :expires-at)))
        (5am:is-true (io.github.cl-sdk.wst.session:renew-session store session-id renewal-ttl-seconds))
        (let ((renewed (io.github.cl-sdk.wst.session:access-session store session-id)))
          (5am:is-true renewed)
          (5am:is (> (getf renewed :expires-at) before))
          (5am:is-true (io.github.cl-sdk.wst.session:session-exists-p store session-id))))
      (io.github.cl-sdk.wst.session:terminate-session store session-id)
      (5am:is-false (io.github.cl-sdk.wst.session:session-exists-p store session-id))
      (5am:is-false (io.github.cl-sdk.wst.session:access-session store session-id)))
    (let* ((immediate-expiry-ttl 0)
           (created (io.github.cl-sdk.wst.session:create-session
                     store
                     '(:user "bob")
                     :session-id "expired-session"
                     :ttl-seconds immediate-expiry-ttl))
           (session-id (getf created :id)))
      (5am:is-false (io.github.cl-sdk.wst.session:access-session store session-id))
      (5am:is-false (io.github.cl-sdk.wst.session:session-exists-p store session-id)))))
