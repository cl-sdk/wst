(in-package :io.github.cl-sdk.wst.test)

(5am:def-suite wst.request-accept.suite
  :description "Tests for the wst.request-accept package.")

(5am:in-suite wst.request-accept.suite)

(5am:def-test parse-request-accept-with-single-media-range ()
  (5am:is (equal '((:|text/html|))
                 (io.github.cl-sdk.wst.request-accept:parse-request-accept
                  "text/html"))))

(5am:def-test parse-request-accept-with-q-and-wildcards ()
  (5am:is (equal '((:|text/html| ("q" . "1.0"))
                   (:|application/json| ("q" . "0.9"))
                   (:|*/*| ("q" . "0.8")))
                 (io.github.cl-sdk.wst.request-accept:parse-request-accept
                  "text/html; q=1.0, application/json; q=0.9, */*;q=0.8"))))

(5am:def-test parse-request-accept-unquotes-parameter-values ()
  (5am:is (equal '((:|application/xml| ("version" . "1") ("q" . "0.7")))
                 (io.github.cl-sdk.wst.request-accept:parse-request-accept
                  "application/xml; version=\"1\"; q=0.7"))))

(5am:def-test parse-request-accept-returns-nil-for-blank-header ()
  (5am:is (null (io.github.cl-sdk.wst.request-accept:parse-request-accept "   "))))

(5am:def-test parse-request-accept-supports-parameter-without-value ()
  (5am:is (equal '((:|text/plain| ("foo" . "")))
                 (io.github.cl-sdk.wst.request-accept:parse-request-accept
                  "text/plain; foo"))))

(5am:def-test parse-request-accept-signals-on-malformed-q-value ()
  (5am:signals error
    (io.github.cl-sdk.wst.request-accept:parse-request-accept
     "text/plain;q=abc, application/json")))

(5am:def-test parse-request-accept-prioritizes-specific-on-equal-quality ()
  (5am:is (equal '((:|application/json| ("q" . "1.0"))
                   (:|text/*| ("q" . "1.0")))
                 (io.github.cl-sdk.wst.request-accept:parse-request-accept
                  "text/*, application/json"))))

(5am:def-test find-best-response-accept-picks-supported-wildcard-type ()
  (5am:is (equal '(:|text/html| ("q" . "1.0"))
                 (io.github.cl-sdk.wst.request-accept:find-best-response-accept
                  '(:|application/json| :|text/html|)
                  '((:|text/*| ("q" . "1.0"))
                    (:|application/*| ("q" . "0.9")))))))

(5am:def-test find-best-response-accept-prefers-specific-media-type ()
  (5am:is (equal '(:|application/json| ("q" . "1.0"))
                 (io.github.cl-sdk.wst.request-accept:find-best-response-accept
                  '(:|application/json| :|text/html|)
                  (io.github.cl-sdk.wst.request-accept:parse-request-accept
                   "text/*, application/json")))))

(5am:def-test find-best-response-accept-ignores-malformed-media-range ()
  (5am:is (equal '(:|application/json| ("q" . "1.0"))
                 (io.github.cl-sdk.wst.request-accept:find-best-response-accept
                  '(:|application/json| :|text/html|)
                  (io.github.cl-sdk.wst.request-accept:parse-request-accept
                   "text, application/json")))))

(5am:def-test find-best-response-accept-falls-back-to-any-response-for-star-star ()
  (5am:is (equal '(:|application/json| ("q" . "0.8"))
                 (io.github.cl-sdk.wst.request-accept:find-best-response-accept
                  '(:|application/json| :|text/html|)
                  '((:|*/*| ("q" . "0.8")))))))

(5am:def-test find-best-response-accept-returns-nil-when-none-matches ()
  (5am:is (null (io.github.cl-sdk.wst.request-accept:find-best-response-accept
                 '(:|application/json|)
                 '((:|text/html| ("q" . "1.0")))))))

(defmethod io.github.cl-sdk.wst.request-accept:respond-with
    ((implementation (eql :|text/plain|)) content request response)
  (io.github.cl-sdk.wst.routing.response.dsl:text content response))

(5am:def-test respond-with-the-appropriate-media ()
  (let ((req (io.github.cl-sdk.wst.routing:make-request :headers (cl-hash-util:hash ("Accept" "text/plain"))
                                                        :data (list :route (io.github.cl-sdk.wst.routing::make-route
                                                                            :name 'a
                                                                            :path "/"
                                                                            :custom '(:response-accepts (:|application/json| :|text/csv|)))
                                                                    :accept (io.github.cl-sdk.wst.request-accept:parse-request-accept "text/plain"))))
        (res (io.github.cl-sdk.wst.routing:make-response)))
    (io.github.cl-sdk.wst.request-accept:respond "ok" req res)
    (5am:is (string-equal "text/plain"
                          (getf (io.github.cl-sdk.wst.routing:response-headers res) :content-type)))
    (5am:is (string-equal "ok" (io.github.cl-sdk.wst.routing:response-content res)))))

(5am:def-test respond-falls-back-to-default-when-route-has-no-accepts ()
  (let ((req (io.github.cl-sdk.wst.routing:make-request :headers (cl-hash-util:hash ("Accept" "application/json"))
                                                        :data (list :route (io.github.cl-sdk.wst.routing::make-route
                                                                            :name 'a
                                                                            :path "/")
                                                                    :accept (io.github.cl-sdk.wst.request-accept:parse-request-accept "application/json"))))
        (res (io.github.cl-sdk.wst.routing:make-response)))
    (io.github.cl-sdk.wst.request-accept:respond "ok" req res)
    (5am:is (string-equal "text/plain"
                          (getf (io.github.cl-sdk.wst.routing:response-headers res) :content-type)))
    (5am:is (string-equal "ok" (io.github.cl-sdk.wst.routing:response-content res)))))

(5am:def-test respond-keeps-response-unchanged-when-implementation-missing ()
  (let ((req (io.github.cl-sdk.wst.routing:make-request :headers (cl-hash-util:hash ("Accept" "application/json"))
                                                        :data (list :route (io.github.cl-sdk.wst.routing::make-route
                                                                            :name 'a
                                                                            :path "/"
                                                                            :custom '(:response-accepts (:|application/json|)))
                                                                    :accept (io.github.cl-sdk.wst.request-accept:parse-request-accept "application/json"))))
        (res (io.github.cl-sdk.wst.routing:make-response :headers '(:x-test "1")
                                                         :content "original")))
    (io.github.cl-sdk.wst.request-accept:respond "new-content" req res)
    (5am:is (equal '(:x-test "1") (io.github.cl-sdk.wst.routing:response-headers res)))
    (5am:is (string= "original" (io.github.cl-sdk.wst.routing:response-content res)))))

;;; RFC 9110 §12.5.1 / RFC 7231 §5.3.2 — q=0 means explicitly not acceptable

(5am:def-test find-best-response-accept-excludes-q-zero-entries ()
  "q=0 makes a media type explicitly not acceptable (RFC 9110 §12.5.1)."
  (5am:is (null (io.github.cl-sdk.wst.request-accept:find-best-response-accept
                 '(:|text/html|)
                 '((:|text/html| ("q" . "0")))))))

(5am:def-test find-best-response-accept-excludes-explicit-q-zero-from-wildcard-match ()
  "q=0 on a specific type must exclude it even when a wildcard would otherwise match (RFC 9110 §12.5.1)."
  (5am:is (equal '(:|application/json| ("q" . "0.8"))
                 (io.github.cl-sdk.wst.request-accept:find-best-response-accept
                  '(:|text/html| :|application/json|)
                  (io.github.cl-sdk.wst.request-accept:parse-request-accept
                   "text/html;q=0, */*;q=0.8")))))

;;; RFC 9110 §12.4.2 / RFC 7231 §5.3.1 — q value must be within 0.000–1.000

(5am:def-test parse-request-accept-signals-on-q-value-above-one ()
  "q > 1 is outside the valid range [0.000, 1.000] (RFC 9110 §12.4.2)."
  (5am:signals error
    (io.github.cl-sdk.wst.request-accept:parse-request-accept
     "text/html;q=1.1")))

(5am:def-test parse-request-accept-signals-on-q-with-too-many-decimals ()
  "q with more than 3 decimal places is malformed (RFC 9110 §12.4.2)."
  (5am:signals error
    (io.github.cl-sdk.wst.request-accept:parse-request-accept
     "text/html;q=0.9999")))

;;; RFC 9110 §12.5.1 — absent Accept header

(5am:def-test parse-request-accept-returns-nil-for-nil-header ()
  "A missing Accept header (nil) should return nil without signalling (RFC 9110 §12.5.1)."
  (5am:is (null (io.github.cl-sdk.wst.request-accept:parse-request-accept nil))))

(5am:def-test find-best-response-accept-returns-first-when-accepts-nil ()
  "An absent Accept header (nil request-accepts) implies the client accepts any media type (RFC 9110 §12.5.1)."
  (5am:is (equal '(:|application/json| ("q" . "1.0"))
                 (io.github.cl-sdk.wst.request-accept:find-best-response-accept
                  '(:|application/json| :|text/html|)
                  nil))))
