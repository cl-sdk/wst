(in-package :io.github.cl-sdk.wst.test)

;;;
;;; wst.routing.response.dsl suite
;;;

(5am:def-suite wst.routing.response.dsl.suite
  :description "Tests for the wst.routing.response.dsl package.")

(5am:in-suite wst.routing.response.dsl.suite)

;;; headers

(5am:def-test set-no-headers-leaves-response-headers-nil ()
  (let ((target (io.github.cl-sdk.wst.routing:make-response)))
    (io.github.cl-sdk.wst.routing.response.dsl:headers nil target)
    (5am:is-true (null (io.github.cl-sdk.wst.routing:response-headers target)))))

(5am:def-test set-a-single-header ()
  (let ((target (io.github.cl-sdk.wst.routing:make-response)))
    (io.github.cl-sdk.wst.routing.response.dsl:headers (list :content-type "mimetype") target)
    (5am:is-true (string-equal (getf (io.github.cl-sdk.wst.routing:response-headers target) :content-type)
                               "mimetype"))))

(5am:def-test setting-headers-replaces-existing-values ()
  (let ((target (io.github.cl-sdk.wst.routing:make-response)))
    (io.github.cl-sdk.wst.routing.response.dsl:headers (list :content-type "mimetype" :content-length 0) target)
    (io.github.cl-sdk.wst.routing.response.dsl:headers (list :content-type "mimetype2") target)
    (5am:is-true (string-equal (getf (io.github.cl-sdk.wst.routing:response-headers target) :content-type)
                               "mimetype2"))))

(5am:def-test headers-returns-the-response-for-chaining ()
  (let* ((target (io.github.cl-sdk.wst.routing:make-response))
         (returned (io.github.cl-sdk.wst.routing.response.dsl:headers (list :x-custom "yes") target)))
    (5am:is (eq target returned))))

;;; status

(5am:def-test set-status-updates-response-status-code ()
  (let ((target (io.github.cl-sdk.wst.routing:make-response)))
    (io.github.cl-sdk.wst.routing.response.dsl:status io.github.cl-sdk.wst.http:+http-status-200+ target)
    (5am:is (= io.github.cl-sdk.wst.http:+http-status-200+ (io.github.cl-sdk.wst.routing:response-status target)))))

(5am:def-test setting-invalid-status-signals-type-error ()
  (5am:signals type-error
    (io.github.cl-sdk.wst.routing.response.dsl:status nil (io.github.cl-sdk.wst.routing:make-response))))

;;; text / html / json body helpers

(5am:def-test set-text-body-sets-content-type-and-content ()
  (let ((target (io.github.cl-sdk.wst.routing.response.dsl:text "hello" (io.github.cl-sdk.wst.routing:make-response))))
    (5am:is (string-equal "text/plain"
                          (getf (io.github.cl-sdk.wst.routing:response-headers target) :content-type)))
    (5am:is (string-equal "hello" (io.github.cl-sdk.wst.routing:response-content target)))))

(5am:def-test set-html-body-sets-content-type-and-content ()
  (let ((target (io.github.cl-sdk.wst.routing.response.dsl:html t "<p>hi</p>" (io.github.cl-sdk.wst.routing:make-response))))
    (5am:is (string-equal "text/html"
                          (getf (io.github.cl-sdk.wst.routing:response-headers target) :content-type)))
    (5am:is (string-equal "<p>hi</p>" (io.github.cl-sdk.wst.routing:response-content target)))))

(5am:def-test set-json-body-sets-content-type-and-content ()
  (let ((target (io.github.cl-sdk.wst.routing.response.dsl:json t "{}" (io.github.cl-sdk.wst.routing:make-response))))
    (5am:is (string-equal "application/json"
                          (getf (io.github.cl-sdk.wst.routing:response-headers target) :content-type)))
    (5am:is (string-equal "{}" (io.github.cl-sdk.wst.routing:response-content target)))))
