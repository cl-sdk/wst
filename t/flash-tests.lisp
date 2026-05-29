(defpackage #:io.github.cl-sdk.wst.flash.test
  (:use #:cl #:fiveam #:io.github.cl-sdk.wst.flash))

(in-package #:io.github.cl-sdk.wst.flash.test)

(def-suite flash-suite)
(in-suite flash-suite)

(defclass test-memory-session-object ()
  ((data :initarg :data :initform nil :accessor test-memory-object-data)))

(defmethod io.github.cl-sdk.wst.flash:flash-messages ((o test-memory-session-object) &key)
  (getf (slot-value o 'data) :flash-messages))

(defmethod io.github.cl-sdk.wst.flash:append-flash-message ((o test-memory-session-object) text &key)
  (let ((messages (getf (slot-value o 'data) :flash-messages)))
    (setf (getf (slot-value o 'data) :flash-messages)
          (append messages (list text)))
    o))

(defmethod io.github.cl-sdk.wst.flash:consume-flash-messages ((o test-memory-session-object) &key)
  (pop (getf (slot-value o 'data) :flash-messages)))

(test must-append-flash-message
  (let ((session-object (make-instance 'test-memory-session-object)))
    (is (equal '() (flash-messages session-object)))
    (append-flash-message session-object "created")
    (let ((messages (flash-messages session-object)))
      (is (= 1 (length messages)))
      (is (equal "created" (first messages))))
    (let ((message (consume-flash-messages session-object))
          (messages (flash-messages session-object)))
      (is (= 0 (length messages)))
      (is (equal "created" message)))))

(test must-read-flash-messages-without-cosuming-it
  (let ((session-object (make-instance 'test-memory-session-object)))
    (append-flash-message session-object "created")
    (let ((messages (flash-messages session-object)))
      (is (= 1 (length messages)))
      (is (equal "created" (first messages))))))

(test must-read-flash-messages-cosuming-the-first
  (let ((session-object (make-instance 'test-memory-session-object)))
    (append-flash-message session-object "created")
    (let ((message (consume-flash-messages session-object))
          (messages (flash-messages session-object)))
      (is (= 0 (length messages)))
      (is (equal "created" message)))))
