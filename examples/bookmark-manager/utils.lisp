(in-package #:wst.example.bookmark-manager)

(defparameter *allowed-username-punctuation* '(#\- #\_ #\.))

(defun trim-whitespace (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun starts-with-http-scheme-p (url)
  (or (and (>= (length url) 7)
           (string-equal "http://" url :end2 7))
      (and (>= (length url) 8)
           (string-equal "https://" url :end2 8))))

(defun normalize-url (url)
  (let ((value (trim-whitespace url)))
    (unless (zerop (length value))
      (if (starts-with-http-scheme-p value)
          value
          (format nil "https://~a" value)))))

(defun valid-http-url-p (url)
  (when (stringp url)
    (let* ((scheme-length (if (and (>= (length url) 7)
                                   (string-equal "http://" url :end2 7))
                              7
                              (if (and (>= (length url) 8)
                                       (string-equal "https://" url :end2 8))
                                  8
                                  nil))))
      (and scheme-length
           (> (length url) scheme-length)
           (not (find-if (lambda (char)
                           (find char '(#\Space #\Tab #\Newline #\Return)))
                         url))
           (let* ((authority+path (subseq url scheme-length))
                  (authority-end (or (position-if (lambda (char)
                                                    (or (char= char #\/)
                                                        (char= char #\?)
                                                        (char= char #\#)))
                                                  authority+path)
                                     (length authority+path)))
                  (authority (subseq authority+path 0 authority-end)))
             (and (not (zerop (length authority)))
                  (not (every (lambda (char) (char= char #\/)) authority))))))))

(defun valid-username-p (user)
  (and (stringp user)
       (<= 1 (length user) 64)
       (every (lambda (char)
                (or (and (<= (char-code #\0) (char-code char) (char-code #\9)))
                    (and (<= (char-code #\A) (char-code char) (char-code #\Z)))
                    (and (<= (char-code #\a) (char-code char) (char-code #\z)))
                    (find char *allowed-username-punctuation*)))
              user)))

(defun bytes-to-hex-string (bytes)
  (with-output-to-string (out)
    (loop for byte across bytes
          do (format out "~2,'0x" byte))))

(defun sqlite-row-column (row index)
  (etypecase row
    (list (nth index row))
    (vector (aref row index))))

(defun parse-positive-integer (raw)
  (when raw
    (handler-case
        (multiple-value-bind (value end)
            (parse-integer raw :junk-allowed t)
          (and value
               (plusp value)
               (= end (length raw))
               value))
      (error () nil))))
