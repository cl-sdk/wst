(defpackage #:io.github.cl-sdk.wst.trace-context.routing
  (:use #:cl #:io.github.cl-sdk.wst.trace-context)
  (:documentation "W3C Trace Context routing adapter for wst.

Provides WRAP-TRACE-CONTEXT, which reads the incoming traceparent and
tracestate headers from a request object, parses them, and injects a
:trace-context value into the request data plist.")
  (:export
   #:wrap-trace-context
   #:trace-context-of
   #:trace-context-propagation-headers))

(in-package #:io.github.cl-sdk.wst.trace-context.routing)

(defun wrap-trace-context (request)
  "Read traceparent and tracestate headers from REQUEST and inject trace context.

The parsed context is stored under :trace-context in the request data plist.

If the incoming traceparent header is absent or invalid a fresh root trace
context is generated. Otherwise a child context is derived from the incoming
one: the trace-id and trace-flags are inherited, and a fresh span-id is
generated as the new parent-id.

Returns REQUEST (mutated in place) for convenient use in middleware chains."
  (let* ((headers (io.github.cl-sdk.wst.routing:request-headers request))
         (traceparent-value (gethash "traceparent" headers))
         (tracestate-value (gethash "tracestate" headers))
         (incoming (when traceparent-value
                     (parse-traceparent traceparent-value)))
         (ctx (if incoming
                  (child-trace-context incoming :tracestate tracestate-value)
                  (new-root-trace-context))))
    (setf (io.github.cl-sdk.wst.routing:request-data request)
          (append (io.github.cl-sdk.wst.routing:request-data request)
                  (list :trace-context ctx)))
    request))

(defun trace-context-of (request)
  "Return the TRACE-CONTEXT injected into REQUEST by WRAP-TRACE-CONTEXT, or NIL."
  (getf (io.github.cl-sdk.wst.routing:request-data request) :trace-context))

(defun trace-context-propagation-headers (ctx)
  "Return a plist of HTTP headers for propagating CTX to downstream services.

Always includes :traceparent. Includes :tracestate when CTX carries a
non-nil tracestate value."
  (let ((headers (list :traceparent (traceparent-string ctx))))
    (when (trace-context-tracestate ctx)
      (setf headers (append headers
                            (list :tracestate (trace-context-tracestate ctx)))))
    headers))
