(defpackage #:wst.request-content
  (:use #:cl)
  (:import-from #:str
                #:split)
  (:import-from #:flexi-streams
                #:make-flexi-stream)
  (:export
   #:parse-content-type
   #:content-as-string
   #:parse-content))

(in-package :wst.request-content)

(defun %unquote-string (s)
  "If S is a quoted-string per RFC 7230, strip the surrounding DQUOTE delimiters
and expand backslash-escaped characters.  Returns S unchanged when not quoted."
  (let ((len (length s)))
    (if (and (>= len 2)
             (char= (char s 0) #\")
             (char= (char s (1- len)) #\"))
        (with-output-to-string (out)
          (loop :with i = 1
                :while (< i (1- len))
                :do (let ((c (char s i)))
                      (if (and (char= c #\\) (< (1+ i) (1- len)))
                          (progn (write-char (char s (1+ i)) out) (incf i 2))
                          (progn (write-char c out) (incf i))))))
        s)))

(defun %parse-mime-options (options-string)
  "Parse a semicolon-separated parameter string into an alist of (\"name\" . \"value\") pairs.

Parameter names are lowercased per RFC 7231 §3.1.1.1 (names are case-insensitive).
Quoted-string parameter values are unquoted per RFC 7230 §3.2.6."
  (loop :for part :in (str:split ";" options-string)
        :for trimmed = (string-trim '(#\Space #\Tab) part)
        :unless (string= trimmed "")
          :collect (let ((pos (position #\= trimmed)))
                     (if pos
                         (let* ((name    (string-downcase
                                          (string-trim '(#\Space #\Tab)
                                                       (subseq trimmed 0 pos))))
                                (raw-val (string-trim '(#\Space #\Tab)
                                                      (subseq trimmed (1+ pos))))
                                (val     (%unquote-string raw-val)))
                           (cons name val))
                         (cons (string-downcase trimmed) "")))))

(defun parse-content-type (content-type)
  "Parse an HTTP Content-Type header value.

Returns a list of (MIME-TYPE-KEYWORD . OPTIONS-ALIST) pairs, one per
comma-separated entry.  MIME-TYPE-KEYWORD is the lowercased MIME type
interned in the keyword package (e.g. :|text/plain|, or
:|application/x-www-form-urlencoded|).  OPTIONS-ALIST is a list of
\(\"name\" . \"value\") string pairs for any parameters (e.g. q, charset).

Examples:
  (parse-content-type \"text/plain\")
  => ((:|text/plain|))

  (parse-content-type \"application/x-www-form-urlencoded; charset=utf-8\")
  => ((:|application/x-www-form-urlencoded| (\"charset\" . \"utf-8\")))

  (parse-content-type \"text/plain, application/x-www-form-urlencoded; q=0.9\")
  => ((:|text/plain|) (:|application/x-www-form-urlencoded| (\"q\" . \"0.9\")))"
  (check-type content-type string)
  (when (not (string= (string-trim '(#\Space #\Tab) content-type) ""))
    (loop :for entry :in (str:split "," content-type)
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

Accepts nil (returns \"\"), a string (returned as-is), a binary or character
stream (read and decoded), or any other value (returned as-is, assumed to be
a string).

ENCODING is the external-format keyword used when decoding a binary stream
\(e.g. :us-ascii, :utf-8).  Defaults to :us-ascii."
  (check-type content string)
  (cond
    ((streamp content)
     (let ((stream (if (subtypep (stream-element-type content) 'character)
                       content
                       (flexi-streams:make-flexi-stream content :external-format encoding))))
       (with-output-to-string (out)
         (loop :for char = (read-char stream nil nil)
               :while char
               :do (write-char char out)))))
    (t content)))

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

TYPE is the MIME type keyword (e.g. :|application/x-www-form-urlencoded|,
:|text/plain|).  CONTENT is the raw body value (string or stream).  ENCODING
is the charset keyword used when decoding a binary stream (e.g. :us-ascii,
:utf-8); it should be the charset extracted from the options alist returned by
parse-content-type and defaults to :us-ascii.

The default method (and the built-in :|text/plain| method) returns CONTENT
coerced to a string without further parsing.  Callers may add methods for
custom MIME types.")
  (:method ((type (eql :|application/x-www-form-urlencoded|)) content
            &optional (encoding :us-ascii))
    (%parse-form-urlencoded (content-as-string content encoding)))
  (:method ((type (eql :|text/plain|)) content &optional (encoding :us-ascii))
    (content-as-string content encoding))
  (:method (type content &optional (encoding :us-ascii))
    (declare (ignore type))
    (content-as-string content encoding)))
