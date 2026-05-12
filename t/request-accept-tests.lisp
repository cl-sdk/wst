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
