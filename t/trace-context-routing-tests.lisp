(defpackage #:io.github.cl-sdk.wst.trace-context.routing.test
  (:use #:cl #:fiveam
        #:io.github.cl-sdk.wst.trace-context
        #:io.github.cl-sdk.wst.trace-context.routing))

(in-package #:io.github.cl-sdk.wst.trace-context.routing.test)

(defun make-request-with-headers (headers-alist)
  "Create a wst request with HEADERS-ALIST loaded into a hash-table."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (pair headers-alist)
      (setf (gethash (car pair) h) (cdr pair)))
    (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET :headers h)))

(def-suite trace-context-routing-suite)
(in-suite trace-context-routing-suite)

;;; wrap-trace-context — incoming traceparent present and valid

(test wrap-injects-trace-context-into-request-data
  (let* ((req (make-request-with-headers
               '(("traceparent" . "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))))
         (req (wrap-trace-context req))
         (ctx (trace-context-of req)))
    (is-true ctx)))

(test wrap-inherits-trace-id-from-incoming
  (let* ((req (make-request-with-headers
               '(("traceparent" . "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))))
         (ctx (trace-context-of (wrap-trace-context req))))
    (is (string= "4bf92f3577b34da6a3ce929d0e0e4736" (trace-context-trace-id ctx)))))

(test wrap-generates-new-span-id-for-current-service
  (let* ((req (make-request-with-headers
               '(("traceparent" . "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))))
         (ctx (trace-context-of (wrap-trace-context req))))
    ;; Current span-id must differ from the incoming parent-id
    (is (not (string= "00f067aa0ba902b7" (trace-context-parent-id ctx))))))

(test wrap-inherits-trace-flags
  (let* ((req (make-request-with-headers
               '(("traceparent" . "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"))))
         (ctx (trace-context-of (wrap-trace-context req))))
    (is (string= "01" (trace-context-trace-flags ctx)))))

(test wrap-propagates-tracestate-from-header
  (let* ((req (make-request-with-headers
               '(("traceparent" . "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
                 ("tracestate"  . "rojo=abc,congo=xyz"))))
         (ctx (trace-context-of (wrap-trace-context req))))
    (is (string= "rojo=abc,congo=xyz" (trace-context-tracestate ctx)))))

;;; wrap-trace-context — no incoming traceparent

(test wrap-creates-fresh-root-when-no-traceparent
  (let* ((req (make-request-with-headers '()))
         (ctx (trace-context-of (wrap-trace-context req))))
    (is-true ctx)
    (is (string= "00" (trace-context-version ctx)))
    (is (= 32 (length (trace-context-trace-id ctx))))
    (is (= 16 (length (trace-context-parent-id ctx))))))

(test wrap-creates-fresh-root-when-traceparent-is-invalid
  (let* ((req (make-request-with-headers
               '(("traceparent" . "not-a-valid-traceparent"))))
         (ctx (trace-context-of (wrap-trace-context req))))
    (is-true ctx)
    (is (= 32 (length (trace-context-trace-id ctx))))))

(test wrap-creates-fresh-root-when-traceparent-has-all-zero-trace-id
  (let* ((req (make-request-with-headers
               '(("traceparent" . "00-00000000000000000000000000000000-00f067aa0ba902b7-01"))))
         (ctx (trace-context-of (wrap-trace-context req))))
    ;; All-zero trace-id is invalid → fresh root with a new trace-id
    (is (not (string= "00000000000000000000000000000000"
                      (trace-context-trace-id ctx))))))

;;; trace-context-of

(test trace-context-of-returns-nil-when-not-wrapped
  (let ((req (io.github.cl-sdk.wst.routing:make-request :uri "/" :method :GET)))
    (is-false (trace-context-of req))))

;;; trace-context-propagation-headers

(test propagation-headers-include-traceparent
  (let* ((ctx (new-root-trace-context))
         (headers (trace-context-propagation-headers ctx)))
    (is-true (getf headers :traceparent))
    (is-true (parse-traceparent (getf headers :traceparent)))))

(test propagation-headers-traceparent-matches-ctx
  (let* ((ctx (make-trace-context :version "00"
                                  :trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
                                  :parent-id "00f067aa0ba902b7"
                                  :trace-flags "01"))
         (headers (trace-context-propagation-headers ctx)))
    (is (string= "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
                 (getf headers :traceparent)))))

(test propagation-headers-omit-tracestate-when-nil
  (let* ((ctx (new-root-trace-context))
         (headers (trace-context-propagation-headers ctx)))
    (is-false (getf headers :tracestate))))

(test propagation-headers-include-tracestate-when-present
  (let* ((ctx (make-trace-context :version "00"
                                  :trace-id "4bf92f3577b34da6a3ce929d0e0e4736"
                                  :parent-id "00f067aa0ba902b7"
                                  :trace-flags "01"
                                  :tracestate "rojo=abc"))
         (headers (trace-context-propagation-headers ctx)))
    (is (string= "rojo=abc" (getf headers :tracestate)))))

;;; end-to-end: wrap then propagate

(test end-to-end-wrap-then-propagate
  (let* ((req (make-request-with-headers
               '(("traceparent" . "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")
                 ("tracestate"  . "vendor=val"))))
         (ctx (trace-context-of (wrap-trace-context req)))
         (out-headers (trace-context-propagation-headers ctx))
         (out-tp (parse-traceparent (getf out-headers :traceparent))))
    ;; Trace-id preserved end-to-end
    (is (string= "4bf92f3577b34da6a3ce929d0e0e4736" (trace-context-trace-id out-tp)))
    ;; Sampled flag preserved
    (is-true (trace-context-sampled-p out-tp))
    ;; Tracestate preserved
    (is (string= "vendor=val" (getf out-headers :tracestate)))))
