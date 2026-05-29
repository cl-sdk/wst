(defpackage #:io.github.cl-sdk.wst.session.csrf.test
  (:use #:cl #:fiveam #:io.github.cl-sdk.wst.session.csrf))

(in-package #:io.github.cl-sdk.wst.session.csrf.test)

(def-suite session-csrf-suite
  :description "Tests for the wst.session.csrf package.")

(in-suite session-csrf-suite)

(defclass test-csrf-context ()
  ((csrf-token :initform nil
               :accessor test-csrf-context-token)))

(defmethod session-csrf-token ((obj test-csrf-context) &key &allow-other-keys)
  (test-csrf-context-token obj))

(defmethod add-session-csrf-token ((obj test-csrf-context) key &key &allow-other-keys)
  (setf (test-csrf-context-token obj) key))

(defmethod remove-session-csrf-token ((obj test-csrf-context) &key &allow-other-keys)
  (setf (test-csrf-context-token obj) nil))

(defmethod verify-session-csrf-token ((obj test-csrf-context) key &key &allow-other-keys)
  (let ((stored (test-csrf-context-token obj)))
    (and stored key (string= stored key))))

(5am:def-test session-csrf-token-lifecycle ()
  (let ((context (make-instance 'test-csrf-context)))
    (5am:is-false (session-csrf-token context))
    (5am:is-false (verify-session-csrf-token context "csrf-1"))

    (5am:is (string= "csrf-1"
                     (add-session-csrf-token context "csrf-1")))
    (5am:is (string= "csrf-1"
                     (session-csrf-token context)))
    (5am:is-true (verify-session-csrf-token context "csrf-1"))
    (5am:is-false (verify-session-csrf-token context "csrf-2"))

    (5am:is-false (remove-session-csrf-token context))
    (5am:is-false (session-csrf-token context))
    (5am:is-false (verify-session-csrf-token context "csrf-1"))))

(5am:def-test session-csrf-token-can-be-replaced ()
  (let ((context (make-instance 'test-csrf-context)))
    (add-session-csrf-token context "csrf-old")
    (5am:is-true (verify-session-csrf-token context "csrf-old"))

    (add-session-csrf-token context "csrf-new")
    (5am:is-false (verify-session-csrf-token context "csrf-old"))
    (5am:is-true (verify-session-csrf-token context "csrf-new"))))
