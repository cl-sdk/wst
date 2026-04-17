(defpackage #:wst.request-accept-content
  (:use #:cl)
  (:import-from #:str
                #:split)
  (:import-from #:flexi-streams
                #:make-flexi-stream)
  (:export
   #:parse-accept
   #:content-as-string
   #:parse-content))

(in-package :wst.request-accept-content)

(defun %parse-mime-options (options-string)
  "Parse a semicolon-separated parameter string into an alist of (\"name\" . \"value\") pairs."
  (loop :for part :in (str:split ";" options-string)
        :for trimmed = (string-trim '(#\Space #\Tab) part)
        :unless (string= trimmed "")
          :collect (let ((pos (position #\= trimmed)))
                     (if pos
                         (cons (string-trim '(#\Space #\Tab) (subseq trimmed 0 pos))
                               (string-trim '(#\Space #\Tab) (subseq trimmed (1+ pos))))
                         (cons trimmed "")))))

(defun parse-accept (accept)
  "Parse an HTTP Accept (or Content-Type) header value.

Returns a list of (MIME-TYPE-KEYWORD . OPTIONS-ALIST) pairs, one per
comma-separated entry.  MIME-TYPE-KEYWORD is the lowercased MIME type
interned in the keyword package (e.g. :|text/plain|, or
:|application/x-www-form-urlencoded|).  OPTIONS-ALIST is a list of
\(\"name\" . \"value\") string pairs for any parameters (e.g. q, charset).

Examples:
  (parse-accept \"text/plain\")
  => ((:|text/plain|))

  (parse-accept \"application/x-www-form-urlencoded; charset=utf-8\")
  => ((:|application/x-www-form-urlencoded| (\"charset\" . \"utf-8\")))

  (parse-accept \"text/plain, application/x-www-form-urlencoded; q=0.9\")
  => ((:|text/plain|) (:|application/x-www-form-urlencoded| (\"q\" . \"0.9\")))"
  (when (and accept
             (not (string= (string-trim '(#\Space #\Tab) accept) "")))
    (loop :for entry :in (str:split "," accept)
          :for trimmed = (string-trim '(#\Space #\Tab) entry)
          :unless (string= trimmed "")
            :collect (let* ((semi (position #\; trimmed))
                            (mime (string-trim '(#\Space #\Tab)
                                               (if semi
                                                   (subseq trimmed 0 semi)
                                                   trimmed)))
                            (opts (when semi
                                    (%parse-mime-options (subseq trimmed (1+ semi))))))
                       (cons (intern (string-downcase mime) :keyword) opts)))))

(defun content-as-string (content &optional (encoding :us-ascii))
  "Normalize CONTENT to a string.

Accepts a string, character or binary stream, or any printable value.
Returns an empty string for nil.

ENCODING is the external-format keyword used when decoding a binary stream
\(e.g. :us-ascii, :utf-8).  Defaults to :us-ascii."
  (cond
    ((null content) "")
    ((stringp content) content)
    ((streamp content)
     (let ((stream (if (subtypep (stream-element-type content) 'character)
                       content
                       (flexi-streams:make-flexi-stream content :external-format encoding))))
       (with-output-to-string (out)
         (loop :for char = (read-char stream nil nil)
               :while char
               :do (write-char char out)))))
    (t (format nil "~a" content))))

(defun %parse-form-urlencoded (content)
  "Parse CONTENT in application/x-www-form-urlencoded format."
  (flet ((url-decode-component (value)
           (let ((size (length value))
                 (index 0))
             (with-output-to-string (out)
               (loop :while (< index size)
                     :do (let ((char (char value index)))
                           (cond
                             ((char= char #\+)
                              (write-char #\Space out)
                              (incf index))
                             ((and (char= char #\%)
                                   (< (+ index 2) size))
                              (let* ((h1 (digit-char-p (char value (1+ index)) 16))
                                     (h2 (digit-char-p (char value (+ index 2)) 16)))
                                (if (and h1 h2)
                                    (progn
                                      (write-char (code-char (+ (* h1 16) h2)) out)
                                      (incf index 3))
                                    (progn
                                      (write-char char out)
                                      (incf index)))))
                             (t
                              (write-char char out)
                              (incf index)))))))))
    (loop :for pair :in (str:split "&" content)
          :unless (string= pair "")
            :collect (let ((separator (position #\= pair)))
                       (if separator
                           (cons (url-decode-component (subseq pair 0 separator))
                                 (url-decode-component (subseq pair (1+ separator))))
                           (cons (url-decode-component pair) ""))))))

(defgeneric parse-content (type content &optional encoding)
  (:documentation "Parse CONTENT using the parser identified by TYPE.

TYPE is the exact MIME type keyword (e.g. :|application/x-www-form-urlencoded|)
or the special symbol :raw for pass-through.  CONTENT is the raw body value
\(string or stream).  ENCODING is the charset keyword used when decoding a
binary stream (e.g. :us-ascii, :utf-8); it should be extracted from the
Content-Type header via parse-accept and defaults to :us-ascii.

The built-in :raw method returns CONTENT coerced to a string without further
parsing.  Any unrecognized TYPE signals an error; callers must add a method
for custom MIME types.")
  (:method ((type (eql :|application/x-www-form-urlencoded|)) content
            &optional (encoding :us-ascii))
    (%parse-form-urlencoded (content-as-string content encoding)))
  (:method ((type (eql :raw)) content &optional (encoding :us-ascii))
    (content-as-string content encoding))
  (:method (type content &optional encoding)
    (declare (ignore content encoding))
    (error "No parse-content method defined for MIME type: ~a" type)))
