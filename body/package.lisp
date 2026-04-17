(defpackage #:wst.body
  (:use #:cl)
  (:import-from #:str
                #:split)
  (:import-from #:flexi-streams
                #:make-flexi-stream)
  (:import-from #:com.inuoe.jzon
                #:parse)
  (:import-from #:uiop
                #:read-file-string)
  (:export
   #:content-type->parser
   #:content-as-string
   #:parse-body))

(in-package :wst.body)

(defun content-type->parser (content-type)
  "Map CONTENT-TYPE string to a parser keyword.

Returns :json for application/json, :form-urlencoded for
application/x-www-form-urlencoded, and :raw for anything else
(including nil or empty string)."
  (let ((normalized (and content-type
                         (string-trim '(#\Space #\Tab)
                                      (car (str:split ";" (string-downcase content-type)))))))
    (cond
      ((or (null normalized) (string= normalized "")) :raw)
      ((string= normalized "application/json") :json)
      ((string= normalized "application/x-www-form-urlencoded") :form-urlencoded)
      (t :raw))))

(defun content-as-string (content)
  "Normalize CONTENT to a UTF-8 string.

Accepts a string, pathname, character or binary stream, or any
printable value.  Returns an empty string for nil."
  (cond
    ((null content) "")
    ((stringp content) content)
    ((pathnamep content) (uiop:read-file-string content))
    ((streamp content)
     (let ((stream (if (subtypep (stream-element-type content) 'character)
                       content
                       (flexi-streams:make-flexi-stream content :external-format :utf-8))))
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

(defgeneric parse-body (type content)
  (:documentation "Parse CONTENT using the parser identified by TYPE.

TYPE is a keyword returned by CONTENT-TYPE->PARSER.
CONTENT is the raw body value (string, pathname, stream, etc.).
All state must be supplied as arguments; no request object is accessed.")
  (:method ((type (eql :raw)) content)
    content)
  (:method ((type (eql :json)) content)
    (com.inuoe.jzon:parse (content-as-string content)))
  (:method ((type (eql :form-urlencoded)) content)
    (%parse-form-urlencoded (content-as-string content)))
  (:method ((type t) content)
    (declare (ignore content))
    (error "Unknown body parser type: ~s" type)))
