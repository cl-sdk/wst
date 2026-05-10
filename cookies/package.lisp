(defpackage #:io.github.cl-sdk.wst.cookies
  (:use #:cl)
  (:import-from #:cl-cookie
                #:make-cookie
                #:cookie
                #:cookie-name
                #:cookie-value
                #:cookie-expires
                #:cookie-path
                #:cookie-domain
                #:cookie-same-site
                #:cookie-max-age
                #:cookie-partitioned
                #:cookie-secure-p
                #:cookie-httponly-p
                #:cookie-origin-host
                #:make-cookie-jar
                #:cookie-jar
                #:cookie-jar-cookies
                #:cookie-jar-host-cookies
                #:merge-cookies
                #:write-cookie-header
                #:write-set-cookie-header
                #:parse-set-cookie-header)
  (:export
   ;; wst.cookies own API
   #:parse-cookies-string
   #:parse-cookies
   ;; re-exported from cl-cookie
   #:make-cookie
   #:cookie
   #:cookie-name
   #:cookie-value
   #:cookie-expires
   #:cookie-path
   #:cookie-domain
   #:cookie-same-site
   #:cookie-max-age
   #:cookie-partitioned
   #:cookie-secure-p
   #:cookie-httponly-p
   #:cookie-origin-host
   #:make-cookie-jar
   #:cookie-jar
   #:cookie-jar-cookies
   #:cookie-jar-host-cookies
   #:merge-cookies
   #:write-cookie-header
   #:write-set-cookie-header
   #:parse-set-cookie-header))

(in-package :io.github.cl-sdk.wst.cookies)

(declaim (ftype (function (string) list)
                parse-cookies-string))
(defun parse-cookies-string (cookies)
  "Parse a Cookie request header string into a list of cl-cookie:cookie instances.

Each NAME=VALUE pair separated by \"; \" becomes one cookie struct.
Pairs without a \"=\" separator are skipped."
  (let ((results '()))
    (labels ((push-pair (start end)
               (let* ((pair (string-trim '(#\Space #\Tab #\Return #\Linefeed)
                                         (subseq cookies start end)))
                      (eq-pos (position #\= pair)))
                 (when (and eq-pos (> eq-pos 0))
                   (push (make-cookie :name (subseq pair 0 eq-pos)
                                      :value (subseq pair (1+ eq-pos))
                                      :sanity-check nil)
                         results)))))
      (loop :with start := 0
            :for i :from 0 :below (length cookies)
            :when (char= (char cookies i) #\;)
              :do (push-pair start i)
                  (setf start (1+ i))
            :finally (push-pair start (length cookies))))
    (nreverse results)))

(declaim (ftype (function (hash-table) list)
                parse-cookies))
(defun parse-cookies (headers)
  "Extract the Cookie header from HEADERS and return a list of cl-cookie:cookie structs."
  (let ((cookies-string (gethash "cookie" headers (gethash "Cookie" headers ""))))
    (parse-cookies-string cookies-string)))
