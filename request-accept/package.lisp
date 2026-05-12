(defpackage #:io.github.cl-sdk.wst.request-accept
  (:use #:cl)
  (:import-from #:str
                #:split)
  (:export
   #:parse-request-accept))

(in-package :io.github.cl-sdk.wst.request-accept)

;; Visible ASCII range: "!" (33) to "~" (126), used for quoted-pair validation.
(defconstant +ascii-printable-start+ 33)
(defconstant +ascii-printable-end+ 126)
;; C0 control upper bound (US, 31) used to reject control chars except HTAB.
(defconstant +ascii-control-end+ 31)

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

(defun parse-request-accept (accept-header)
  "Parse an HTTP Accept header into media-range entries.

Returns a list of (MEDIA-RANGE-KEYWORD . PARAMETERS-ALIST) pairs.
MEDIA-RANGE-KEYWORD is interned in the keyword package and lowercased
  \(e.g. :|text/html|, :|application/json|, :|*/*|). PARAMETERS-ALIST is
  an alist of (\"name\" . \"value\") string conses; valueless parameters use
  an empty string as value."
  (check-type accept-header string)
  (unless (string= (string-trim '(#\Space #\Tab) accept-header) "")
    (loop :for entry :in (split "," accept-header)
          :for trimmed-entry = (string-trim '(#\Space #\Tab) entry)
          :unless (string= trimmed-entry "")
            :collect (let* ((sections (split ";" trimmed-entry))
                            (media-range (string-trim '(#\Space #\Tab) (car sections)))
                            (params (%parse-accept-parameters (cdr sections))))
                       (cons (intern (string-downcase media-range) :keyword) params)))))
