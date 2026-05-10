(in-package :io.github.cl-sdk.wst.test)

;;;
;;; wst.request-content suite
;;;

(5am:def-suite wst.request-content.suite
  :description "Tests for the wst.request-content package.")

(5am:in-suite wst.request-content.suite)

;;; parse-content – user-defined parser

;; Register a user-defined parser for a custom MIME type at suite load time.
(defmethod io.github.cl-sdk.wst.request-content:parse-content
    ((type (eql :|application/x-custom|)) content &optional (encoding :us-ascii))
  "Example user-defined parser: upper-cases the raw body string."
  (string-upcase (io.github.cl-sdk.wst.request-content:content-as-string content encoding)))

(5am:def-test user-defined-parser-is-called-for-custom-mime-type ()
  "Dispatching parse-content on a user-defined MIME keyword calls the custom method."
  (5am:is (string= "HELLO"
                   (io.github.cl-sdk.wst.request-content:parse-content
                    :|application/x-custom| "hello"))))

(5am:def-test user-defined-parser-receives-stream-content ()
  "The user-defined parser can call content-as-string to normalise its input."
  (let ((stream (make-string-input-stream "world")))
    (5am:is (string= "WORLD"
                     (io.github.cl-sdk.wst.request-content:parse-content
                      :|application/x-custom| stream)))))
