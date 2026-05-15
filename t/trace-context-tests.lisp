(defpackage #:io.github.cl-sdk.wst.trace-context.test
  (:use #:cl #:fiveam #:io.github.cl-sdk.wst.trace-context))

(in-package #:io.github.cl-sdk.wst.trace-context.test)

(def-suite trace-context-suite)
(in-suite trace-context-suite)

;;; parse-traceparent — valid inputs

(test parse-traceparent-returns-struct-on-valid-input
  (let ((ctx (parse-traceparent
              "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")))
    (is-true ctx)
    (is (string= "00" (trace-context-version ctx)))
    (is (string= "4bf92f3577b34da6a3ce929d0e0e4736" (trace-context-trace-id ctx)))
    (is (string= "00f067aa0ba902b7" (trace-context-parent-id ctx)))
    (is (string= "01" (trace-context-trace-flags ctx)))))

(test parse-traceparent-trims-surrounding-whitespace
  (is-true (parse-traceparent
            "  00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01  ")))

(test parse-traceparent-accepts-extra-fields-for-forward-compatibility
  (is-true (parse-traceparent
            "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01-extra")))

(test parse-traceparent-sampled-flag-off
  (let ((ctx (parse-traceparent
              "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00")))
    (is-true ctx)
    (is (string= "00" (trace-context-trace-flags ctx)))))

;;; parse-traceparent — invalid inputs

(test parse-traceparent-returns-nil-on-nil
  (is-false (parse-traceparent nil)))

(test parse-traceparent-returns-nil-on-empty-string
  (is-false (parse-traceparent "")))

(test parse-traceparent-returns-nil-on-wrong-version
  (is-false (parse-traceparent
             "01-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")))

(test parse-traceparent-returns-nil-on-version-ff
  (is-false (parse-traceparent
             "ff-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")))

(test parse-traceparent-returns-nil-when-trace-id-all-zeros
  (is-false (parse-traceparent
             "00-00000000000000000000000000000000-00f067aa0ba902b7-01")))

(test parse-traceparent-returns-nil-when-parent-id-all-zeros
  (is-false (parse-traceparent
             "00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-01")))

(test parse-traceparent-returns-nil-on-short-trace-id
  (is-false (parse-traceparent
             "00-4bf92f3577b34da6a3ce929d0e0-00f067aa0ba902b7-01")))

(test parse-traceparent-returns-nil-on-short-parent-id
  (is-false (parse-traceparent
             "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0b-01")))

(test parse-traceparent-returns-nil-on-uppercase-hex
  (is-false (parse-traceparent
             "00-4BF92F3577B34DA6A3CE929D0E0E4736-00F067AA0BA902B7-01")))

(test parse-traceparent-returns-nil-on-missing-fields
  (is-false (parse-traceparent "00-4bf92f3577b34da6a3ce929d0e0e4736")))

(test parse-traceparent-returns-nil-on-non-hex-chars
  (is-false (parse-traceparent
             "00-4bf92f3577b34da6a3ce929d0e0e473z-00f067aa0ba902b7-01")))

;;; parse-tracestate

(test parse-tracestate-returns-nil-on-nil
  (is-false (parse-tracestate nil)))

(test parse-tracestate-returns-nil-on-empty-string
  (is-false (parse-tracestate "")))

(test parse-tracestate-single-entry
  (let ((ts (parse-tracestate "rojo=00f067aa0ba902b7")))
    (is (= 1 (length ts)))
    (is (string= "rojo" (caar ts)))
    (is (string= "00f067aa0ba902b7" (cdar ts)))))

(test parse-tracestate-multiple-entries
  (let ((ts (parse-tracestate "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")))
    (is (= 2 (length ts)))
    (is (string= "rojo" (car (first ts))))
    (is (string= "congo" (car (second ts))))))

(test parse-tracestate-trims-whitespace-around-entries
  (let ((ts (parse-tracestate " rojo=abc , congo=xyz ")))
    (is (= 2 (length ts)))
    (is (string= "rojo" (car (first ts))))))

;;; make-span-id / make-trace-id

(test make-span-id-produces-16-lowercase-hex-chars
  (let ((id (make-span-id)))
    (is (= 16 (length id)))
    (is (every (lambda (c) (or (char<= #\0 c #\9) (char<= #\a c #\f))) id))))

(test make-trace-id-produces-32-lowercase-hex-chars
  (let ((id (make-trace-id)))
    (is (= 32 (length id)))
    (is (every (lambda (c) (or (char<= #\0 c #\9) (char<= #\a c #\f))) id))))

(test make-span-id-generates-different-values
  (is (not (string= (make-span-id) (make-span-id)))))

(test make-trace-id-generates-different-values
  (is (not (string= (make-trace-id) (make-trace-id)))))

;;; new-root-trace-context

(test new-root-trace-context-has-version-00
  (is (string= "00" (trace-context-version (new-root-trace-context)))))

(test new-root-trace-context-has-valid-trace-id
  (let ((ctx (new-root-trace-context)))
    (is (= 32 (length (trace-context-trace-id ctx))))))

(test new-root-trace-context-has-valid-span-id
  (let ((ctx (new-root-trace-context)))
    (is (= 16 (length (trace-context-parent-id ctx))))))

(test new-root-trace-context-has-zero-flags
  (is (string= "00" (trace-context-trace-flags (new-root-trace-context)))))

(test new-root-trace-context-has-no-tracestate
  (is-false (trace-context-tracestate (new-root-trace-context))))

;;; child-trace-context

(test child-trace-context-inherits-trace-id
  (let* ((parent (parse-traceparent
                  "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))
         (child (child-trace-context parent)))
    (is (string= (trace-context-trace-id parent)
                 (trace-context-trace-id child)))))

(test child-trace-context-inherits-trace-flags
  (let* ((parent (parse-traceparent
                  "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))
         (child (child-trace-context parent)))
    (is (string= (trace-context-trace-flags parent)
                 (trace-context-trace-flags child)))))

(test child-trace-context-generates-new-parent-id
  (let* ((parent (parse-traceparent
                  "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))
         (child (child-trace-context parent)))
    (is (not (string= (trace-context-parent-id parent)
                      (trace-context-parent-id child))))))

(test child-trace-context-inherits-tracestate-from-parent
  (let* ((parent (parse-traceparent
                  "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))
         (_ (setf (trace-context-tracestate parent) "vendor=abc"))
         (child (child-trace-context parent)))
    (declare (ignore _))
    (is (string= "vendor=abc" (trace-context-tracestate child)))))

(test child-trace-context-overrides-tracestate-when-provided
  (let* ((parent (parse-traceparent
                  "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))
         (child (child-trace-context parent :tracestate "new=xyz")))
    (is (string= "new=xyz" (trace-context-tracestate child)))))

;;; traceparent-string

(test traceparent-string-roundtrip
  (let* ((original "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
         (ctx (parse-traceparent original)))
    (is (string= original (traceparent-string ctx)))))

(test traceparent-string-format
  (let ((ctx (make-trace-context :version "00"
                                 :trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
                                 :parent-id "00f067aa0ba902b7"
                                 :trace-flags "01")))
    (is (string= "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
                 (traceparent-string ctx)))))

;;; tracestate-string

(test tracestate-string-returns-nil-on-nil
  (is-false (tracestate-string nil)))

(test tracestate-string-single-entry
  (is (string= "rojo=abc" (tracestate-string '(("rojo" . "abc"))))))

(test tracestate-string-multiple-entries
  (is (string= "rojo=abc,congo=xyz"
               (tracestate-string '(("rojo" . "abc") ("congo" . "xyz"))))))

(test tracestate-string-roundtrip
  (let* ((original "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
         (parsed (parse-tracestate original)))
    (is (string= original (tracestate-string parsed)))))

;;; trace-context-sampled-p

(test trace-context-sampled-p-returns-true-when-bit-0-set
  (let ((ctx (parse-traceparent
              "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")))
    (is-true (trace-context-sampled-p ctx))))

(test trace-context-sampled-p-returns-false-when-bit-0-not-set
  (let ((ctx (parse-traceparent
              "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00")))
    (is-false (trace-context-sampled-p ctx))))

(test trace-context-sampled-p-checks-only-bit-0
  ;; flags "02" = bit 1 set, bit 0 not set
  (let ((ctx (make-trace-context :version "00"
                                 :trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
                                 :parent-id "00f067aa0ba902b7"
                                 :trace-flags "02")))
    (is-false (trace-context-sampled-p ctx))))
