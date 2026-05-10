(in-package :io.github.cl-sdk.wst.test)

;;;
;;; wst.routing.woo suite
;;;

(5am:def-suite wst.routing.woo.suite
  :description "Tests for the wst.routing.woo adapter package.")

(5am:in-suite wst.routing.woo.suite)

(5am:def-test request-from-woo-env-parses-uri-components ()
  (let* ((env (list :request-uri "/users/1?foo=bar#section"
                    :headers (cl-hash-util:hash ("content-type" "text/html"))
                    :request-method :GET
                    :content-type "text/html"
                    :content-length 42
                    :raw-body nil))
         (req (io.github.cl-sdk.wst.routing.woo:request-from-woo-env env)))
    (5am:is (string-equal "/users/1" (io.github.cl-sdk.wst.routing:request-uri req)))
    (5am:is (string-equal "foo=bar" (io.github.cl-sdk.wst.routing:request-query req)))
    (5am:is (eql :GET (io.github.cl-sdk.wst.routing:request-method req)))
    (5am:is (= 42 (io.github.cl-sdk.wst.routing:request-content-length req)))))

(5am:def-test request-from-woo-env-stores-original-env ()
  (let* ((env (list :request-uri "/"
                    :headers (cl-hash-util:hash)
                    :request-method :POST
                    :content-type nil
                    :content-length 0
                    :raw-body nil))
         (req (io.github.cl-sdk.wst.routing.woo:request-from-woo-env env)))
    (5am:is-true (getf (io.github.cl-sdk.wst.routing:request-data req) :env))
    (5am:is (eq env (getf (io.github.cl-sdk.wst.routing:request-data req) :env)))))

(5am:def-test response-to-woo-response-returns-correct-format ()
  (let* ((rs (io.github.cl-sdk.wst.routing:make-response)))
    (setf (io.github.cl-sdk.wst.routing:response-status rs) 200
          (io.github.cl-sdk.wst.routing:response-headers rs) (list :content-type "text/plain")
          (io.github.cl-sdk.wst.routing:response-content rs) "hello")
    (let ((woo-rs (io.github.cl-sdk.wst.routing.woo:response-to-woo-response rs)))
      (5am:is (= 200 (first woo-rs)))
      (5am:is (equal (list :content-type "text/plain") (second woo-rs)))
      (5am:is (equal (list "hello") (third woo-rs))))))
