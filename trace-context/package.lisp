(defpackage #:io.github.cl-sdk.wst.trace-context
  (:use #:cl)
  (:export
   #:trace-context
   #:make-trace-context
   #:trace-context-version
   #:trace-context-trace-id
   #:trace-context-parent-id
   #:trace-context-trace-flags
   #:trace-context-tracestate
   #:parse-traceparent
   #:parse-tracestate
   #:make-span-id
   #:make-trace-id
   #:new-root-trace-context
   #:child-trace-context
   #:traceparent-string
   #:tracestate-string
   #:trace-context-sampled-p))

(in-package #:io.github.cl-sdk.wst.trace-context)

(defstruct trace-context
  "Represents a parsed W3C Trace Context (https://www.w3.org/TR/trace-context/).

Fields:
  - VERSION: The version byte as a 2-char lowercase hex string. Currently always \"00\".
  - TRACE-ID: 128-bit trace identifier as a 32-char lowercase hex string.
  - PARENT-ID: 64-bit parent span identifier as a 16-char lowercase hex string.
  - TRACE-FLAGS: Trace flags as a 2-char lowercase hex string (bit 0 = sampled).
  - TRACESTATE: Raw value of the tracestate header string, or nil."
  version
  trace-id
  parent-id
  trace-flags
  tracestate)

(defun %split-char (char string)
  "Split STRING by CHAR, returning a list of substrings."
  (loop :for start = 0 :then (1+ end)
        :for end = (position char string :start start)
        :collect (subseq string start (or end (length string)))
        :while end))

(defun %valid-hex-p (string expected-length)
  "Return true if STRING is a lowercase hex string of exactly EXPECTED-LENGTH characters."
  (and (stringp string)
       (= (length string) expected-length)
       (every (lambda (c) (or (char<= #\0 c #\9) (char<= #\a c #\f))) string)))

(defun %all-zeros-p (string)
  "Return true if STRING consists entirely of '0' characters."
  (every (lambda (c) (char= c #\0)) string))

(defun parse-traceparent (header-value)
  "Parse a traceparent header value per the W3C Trace Context spec.

HEADER-VALUE is the string value of the traceparent HTTP header.

Returns a TRACE-CONTEXT struct on success, or NIL if the value is invalid.

Parsing rules:
  - At least four dash-separated fields: version, trace-id, parent-id, trace-flags.
    Extra fields (from future spec versions) are accepted and ignored.
  - Version must be exactly \"00\"; other values are rejected.
  - Trace-id must be 32 lowercase hex chars and must not be all zeros.
  - Parent-id must be 16 lowercase hex chars and must not be all zeros.
  - Trace-flags must be 2 lowercase hex chars."
  (when (and (stringp header-value)
             (not (string= (string-trim '(#\Space #\Tab) header-value) "")))
    (let ((parts (%split-char #\- (string-trim '(#\Space #\Tab) header-value))))
      (when (>= (length parts) 4)
        (destructuring-bind (version trace-id parent-id trace-flags . _rest)
            parts
          (declare (ignore _rest))
          (when (and (string= version "00")
                     (%valid-hex-p trace-id 32)
                     (not (%all-zeros-p trace-id))
                     (%valid-hex-p parent-id 16)
                     (not (%all-zeros-p parent-id))
                     (%valid-hex-p trace-flags 2))
            (make-trace-context :version version
                                :trace-id trace-id
                                :parent-id parent-id
                                :trace-flags trace-flags)))))))

(defun parse-tracestate (header-value)
  "Parse a tracestate header value per the W3C Trace Context spec.

HEADER-VALUE is the string value of the tracestate HTTP header.

Returns an alist of (VENDOR-KEY . VALUE) string pairs, one per comma-separated
list-member entry. Whitespace around entries is trimmed. Returns NIL if
HEADER-VALUE is NIL or consists only of whitespace."
  (when (and (stringp header-value)
             (not (string= (string-trim '(#\Space #\Tab) header-value) "")))
    (loop :for entry :in (%split-char #\, header-value)
          :for trimmed = (string-trim '(#\Space #\Tab) entry)
          :unless (string= trimmed "")
            :collect (let ((eq-pos (position #\= trimmed)))
                       (if eq-pos
                           (cons (subseq trimmed 0 eq-pos)
                                 (subseq trimmed (1+ eq-pos)))
                           (cons trimmed ""))))))

(defun %random-hex (byte-count)
  "Generate BYTE-COUNT random bytes and return them as a lowercase hex string."
  (with-output-to-string (s)
    (dotimes (i byte-count)
      (format s "~(~2,'0x~)" (random 256)))))

(defun make-span-id ()
  "Generate a random 64-bit span identifier as a 16-char lowercase hex string."
  (%random-hex 8))

(defun make-trace-id ()
  "Generate a random 128-bit trace identifier as a 32-char lowercase hex string."
  (%random-hex 16))

(defun new-root-trace-context ()
  "Create a fresh root TRACE-CONTEXT with a new random trace-id and span-id.

The trace-flags default to \"00\" (not sampled). Tracestate is nil."
  (make-trace-context :version "00"
                      :trace-id (make-trace-id)
                      :parent-id (make-span-id)
                      :trace-flags "00"
                      :tracestate nil))

(defun child-trace-context (parent-ctx &key tracestate)
  "Create a new TRACE-CONTEXT representing the current service's span.

The new context inherits TRACE-ID and TRACE-FLAGS from PARENT-CTX and
generates a fresh random span-id as the PARENT-ID.

TRACESTATE, when provided, overrides the tracestate inherited from
PARENT-CTX. Pass NIL explicitly to suppress tracestate propagation."
  (make-trace-context :version "00"
                      :trace-id (trace-context-trace-id parent-ctx)
                      :parent-id (make-span-id)
                      :trace-flags (trace-context-trace-flags parent-ctx)
                      :tracestate (or tracestate (trace-context-tracestate parent-ctx))))

(defun traceparent-string (ctx)
  "Serialize CTX to a traceparent header value string.

Returns a string of the form \"version-trace-id-parent-id-trace-flags\"."
  (format nil "~a-~a-~a-~a"
          (trace-context-version ctx)
          (trace-context-trace-id ctx)
          (trace-context-parent-id ctx)
          (trace-context-trace-flags ctx)))

(defun tracestate-string (tracestate)
  "Serialize a tracestate alist to a tracestate header value string.

TRACESTATE is an alist of (VENDOR-KEY . VALUE) string pairs as returned by
PARSE-TRACESTATE. Returns NIL when TRACESTATE is NIL or empty."
  (when tracestate
    (format nil "~{~a=~a~^,~}"
            (loop :for (k . v) :in tracestate
                  :append (list k v)))))

(defun trace-context-sampled-p (ctx)
  "Return true if the sampled flag (bit 0 of trace-flags) is set in CTX."
  (logbitp 0 (parse-integer (trace-context-trace-flags ctx) :radix 16)))
