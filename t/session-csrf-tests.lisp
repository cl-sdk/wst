(in-package :io.github.cl-sdk.wst.test)

(5am:def-suite wst.session-csrf.suite
  :description "Tests for the wst.session.csrf package.")

(5am:in-suite wst.session-csrf.suite)

(defclass test-csrf-context ()
  ((csrf-token :initform nil
               :accessor test-csrf-context-token)))

(defmethod io.github.cl-sdk.wst.session.csrf:session-csrf-token ((obj test-csrf-context) &key &allow-other-keys)
  (test-csrf-context-token obj))

(defmethod io.github.cl-sdk.wst.session.csrf:add-session-csrf-token ((obj test-csrf-context) key &key &allow-other-keys)
  (setf (test-csrf-context-token obj) key))

(defmethod io.github.cl-sdk.wst.session.csrf:remove-session-csrf-token ((obj test-csrf-context) &key &allow-other-keys)
  (setf (test-csrf-context-token obj) nil))

(defmethod io.github.cl-sdk.wst.session.csrf:verify-session-csrf-token ((obj test-csrf-context) key &key &allow-other-keys)
  (let ((stored (test-csrf-context-token obj)))
    (and stored key (string= stored key))))

(5am:def-test session-csrf-token-lifecycle ()
  (let ((context (make-instance 'test-csrf-context)))
    (5am:is-false (io.github.cl-sdk.wst.session.csrf:session-csrf-token context))
    (5am:is-false (io.github.cl-sdk.wst.session.csrf:verify-session-csrf-token context "csrf-1"))

    (5am:is (string= "csrf-1"
                     (io.github.cl-sdk.wst.session.csrf:add-session-csrf-token context "csrf-1")))
    (5am:is (string= "csrf-1"
                     (io.github.cl-sdk.wst.session.csrf:session-csrf-token context)))
    (5am:is-true (io.github.cl-sdk.wst.session.csrf:verify-session-csrf-token context "csrf-1"))
    (5am:is-false (io.github.cl-sdk.wst.session.csrf:verify-session-csrf-token context "csrf-2"))

    (5am:is-false (io.github.cl-sdk.wst.session.csrf:remove-session-csrf-token context))
    (5am:is-false (io.github.cl-sdk.wst.session.csrf:session-csrf-token context))
    (5am:is-false (io.github.cl-sdk.wst.session.csrf:verify-session-csrf-token context "csrf-1"))))

(5am:def-test session-csrf-token-can-be-replaced ()
  (let ((context (make-instance 'test-csrf-context)))
    (io.github.cl-sdk.wst.session.csrf:add-session-csrf-token context "csrf-old")
    (5am:is-true (io.github.cl-sdk.wst.session.csrf:verify-session-csrf-token context "csrf-old"))

    (io.github.cl-sdk.wst.session.csrf:add-session-csrf-token context "csrf-new")
    (5am:is-false (io.github.cl-sdk.wst.session.csrf:verify-session-csrf-token context "csrf-old"))
    (5am:is-true (io.github.cl-sdk.wst.session.csrf:verify-session-csrf-token context "csrf-new"))))
