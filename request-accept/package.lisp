(defpackage #:io.github.cl-sdk.wst.request-accept
  (:use #:cl)
  (:import-from #:str
                #:split)
  (:export
   #:parse-request-accept
   #:find-best-response-accept
   #:respond-with
   #:respond))

(in-package :io.github.cl-sdk.wst.request-accept)

;; Visible ASCII range: "!" (33) to "~" (126), used for quoted-pair validation.
(defconstant +ascii-printable-start+ 33)
(defconstant +ascii-printable-end+ 126)
;; C0 control upper bound (US, 31) used to reject control chars except HTAB.
(defconstant +ascii-control-end+ 31)

(defparameter *default-response-accept* '(:|text/plain| ("q" "1.0")))

(defgeneric respond-with (implementation content request response)
  (:documentation "Render CONTENT according to IMPLEMENTATION (a selected media type).")
  (:method ((implementation t) content request response)
    content))

(defun %find-response-accept-for-type (response-accepts media-type)
  (if (string-equal media-type "*")
      (car response-accepts)
      (find-if (lambda (response-accept)
                 (string-equal media-type
                               (car (split "/" (string response-accept)))))
               response-accepts)))

(defun %find-response-accept (response-accepts request-accept)
  (let* ((mime-type (string (car request-accept)))
         (mime-sub (split "/" mime-type))
         (media-type (first mime-sub))
         (media-subtype (second mime-sub)))
    (cond
      ((and (string-equal "*" media-type)
            (string-equal "*" media-subtype))
       (car response-accepts))
      ((string-equal "*" media-subtype)
       (%find-response-accept-for-type response-accepts media-type))
      (t
       (find (car request-accept) response-accepts :test #'eq)))))

(defun find-best-response-accept (response-accepts request-accepts)
  "Pick the first acceptable response media type supported by RESPONSE-ACCEPTS.

REQUEST-ACCEPTS must be ordered by preference (for example, output from
`parse-request-accept`). Returns the selected accept entry as a
\(MEDIA-RANGE-KEYWORD . PARAMETERS-ALIST) pair, or NIL when no match exists."
  (when (and response-accepts request-accepts)
    (loop :for request-accept :in request-accepts
          :for response-accept = (%find-response-accept response-accepts request-accept)
          :when response-accept
            :return (cons response-accept (cdr request-accept)))))

(defun respond (content request response)
  "Dispatch CONTENT rendering based on request Accept and route response metadata."
  (io.github.cl-sdk.wst.routing:with-request-data (accept route)
      request
    (let* ((request-accept accept)
           (route-accept (getf (io.github.cl-sdk.wst.routing::route-custom route) :response-accepts))
           (mime-type (or (find-best-response-accept route-accept request-accept)
                         *default-response-accept*)))
      (respond-with (car mime-type) content request response))))

(defun %valid-quoted-pair-char-p (c)
  (or (char= c #\Tab)
      (char= c #\Space)
      (<= +ascii-printable-start+ (char-code c) +ascii-printable-end+)))

(defun %valid-quoted-string-p (s)
  (let ((len (length s)))
    (when (and (>= len 2)
               (char= (char s 0) #\")
               (char= (char s (1- len)) #\"))
      (loop :with i = 1
            :with last-index = (1- len)
            :while (< i last-index)
            :do (let ((c (char s i)))
                  (cond
                    ((char= c #\\)
                     (when (>= (1+ i) last-index)
                       (return-from %valid-quoted-string-p nil))
                     (unless (%valid-quoted-pair-char-p (char s (1+ i)))
                       (return-from %valid-quoted-string-p nil))
                     (incf i 2))
                    ((char= c #\")
                     (return-from %valid-quoted-string-p nil))
                    ((and (<= (char-code c) +ascii-control-end+)
                          (not (char= c #\Tab)))
                     (return-from %valid-quoted-string-p nil))
                    (t
                     (incf i)))))
            :finally (return-from %valid-quoted-string-p t))))

(defun %unquote-string (s)
  "If S is a quoted-string, strip surrounding DQUOTE and unescape backslash escapes."
  (let ((len (length s)))
    (if (%valid-quoted-string-p s)
        (with-output-to-string (out)
          (loop :with i = 1
                :while (< i (1- len))
                :do (let ((c (char s i)))
                      (if (char= c #\\)
                          (progn (write-char (char s (1+ i)) out) (incf i 2))
                          (progn (write-char c out) (incf i))))))
        s)))

(defun %parse-accept-parameters (parameters)
  "Parse semicolon-delimited Accept parameters into an alist of string conses."
  (loop :for part :in parameters
        :for trimmed = (string-trim '(#\Space #\Tab) part)
        :unless (string= trimmed "")
          :collect (let ((equal-position (position #\= trimmed)))
                     (if equal-position
                         (let* ((name (string-downcase
                                       (string-trim '(#\Space #\Tab)
                                                    (subseq trimmed 0 equal-position))))
                                (value (%unquote-string
                                        (string-trim '(#\Space #\Tab)
                                                     (subseq trimmed (1+ equal-position))))))
                           (cons name value))
                         (cons (string-downcase trimmed) "")))))

(defun %process-request-accepts (request-accepts)
  (labels ((get-accept-entry-quality-value (entry)
             (or (find-if (lambda (item) (string-equal (car item) "q"))
                         entry)
                '("q" . "1.0"))))
    (sort request-accepts
          (lambda (a b)
            (let* ((qa (get-accept-entry-quality-value (cdr a)))
                   (qb (get-accept-entry-quality-value (cdr b))))
              (> (serapeum:parse-float (cdr qa)) (serapeum:parse-float (cdr qb))))))))

(defun parse-request-accept (accept-header)
  "Parse an HTTP Accept header into media-range entries.

Returns a list of (MEDIA-RANGE-KEYWORD . PARAMETERS-ALIST) pairs.
MEDIA-RANGE-KEYWORD is interned in the keyword package and lowercased
  \(e.g. :|text/html|, :|application/json|, :|*/*|). PARAMETERS-ALIST is
  an alist of (\"name\" . \"value\") string conses; valueless parameters use
  an empty string as value."
  (check-type accept-header string)
  (unless (string= (string-trim '(#\Space #\Tab) accept-header) "")
    (%process-request-accepts
     (loop :for entry :in (split "," accept-header)
           :for trimmed-entry = (string-trim '(#\Space #\Tab) entry)
           :unless (string= trimmed-entry "")
             :collect (let* ((sections (split ";" trimmed-entry))
                             (media-range (string-trim '(#\Space #\Tab) (car sections)))
                             (params (%parse-accept-parameters (cdr sections))))
                        (cons (intern (string-downcase media-range) :keyword) params))))))
