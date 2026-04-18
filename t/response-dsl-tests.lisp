(in-package :wst.routing.test)

;;;
;;; wst.routing.response.dsl suite
;;;

(5am:def-suite wst.routing.response.dsl.suite
  :description "Tests for the wst.routing.response.dsl package.")

(5am:in-suite wst.routing.response.dsl.suite)

;;; headers

(5am:def-test set-no-headers-leaves-response-headers-nil ()
  (let ((target (wst.routing:make-response)))
    (wst.routing.response.dsl:headers nil target)
    (5am:is-true (null (wst.routing:response-headers target)))))

(5am:def-test set-a-single-header ()
  (let ((target (wst.routing:make-response)))
    (wst.routing.response.dsl:headers (list :content-type "mimetype") target)
    (5am:is-true (string-equal (getf (wst.routing:response-headers target) :content-type)
                               "mimetype"))))

(5am:def-test setting-headers-replaces-existing-values ()
  (let ((target (wst.routing:make-response)))
    (wst.routing.response.dsl:headers (list :content-type "mimetype" :content-length 0) target)
    (wst.routing.response.dsl:headers (list :content-type "mimetype2") target)
    (5am:is-true (string-equal (getf (wst.routing:response-headers target) :content-type)
                               "mimetype2"))))

(5am:def-test headers-returns-the-response-for-chaining ()
  (let* ((target (wst.routing:make-response))
         (returned (wst.routing.response.dsl:headers (list :x-custom "yes") target)))
    (5am:is (eq target returned))))

;;; status

(5am:def-test set-status-updates-response-status-code ()
  (let ((target (wst.routing:make-response)))
    (wst.routing.response.dsl:status wst.http:+http-status-200+ target)
    (5am:is (= wst.http:+http-status-200+ (wst.routing:response-status target)))))

(5am:def-test setting-invalid-status-signals-type-error ()
  (5am:signals type-error
    (wst.routing.response.dsl:status nil (wst.routing:make-response))))

;;; text / html / json body helpers

(5am:def-test set-text-body-sets-content-type-and-content ()
  (let ((target (wst.routing.response.dsl:text "hello" (wst.routing:make-response))))
    (5am:is (string-equal "text/plain"
                          (getf (wst.routing:response-headers target) :content-type)))
    (5am:is (string-equal "hello" (wst.routing:response-content target)))))

(5am:def-test set-html-body-sets-content-type-and-content ()
  (let ((target (wst.routing.response.dsl:html t "<p>hi</p>" (wst.routing:make-response))))
    (5am:is (string-equal "text/html"
                          (getf (wst.routing:response-headers target) :content-type)))
    (5am:is (string-equal "<p>hi</p>" (wst.routing:response-content target)))))

(5am:def-test set-json-body-sets-content-type-and-content ()
  (let ((target (wst.routing.response.dsl:json t "{}" (wst.routing:make-response))))
    (5am:is (string-equal "application/json"
                          (getf (wst.routing:response-headers target) :content-type)))
    (5am:is (string-equal "{}" (wst.routing:response-content target)))))
