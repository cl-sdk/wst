(defpackage :io.github.cl-sdk.wst.cors.test
  (:use :cl :fiveam :io.github.cl-sdk.wst.cors))

(in-package :io.github.cl-sdk.wst.cors.test)

(def-suite cors-suite)
(in-suite cors-suite)

(defun header (headers name)
  (cdr (assoc name headers :test #'string=)))

(test simple-request
  (let* ((policy (make-cors-policy
                  :allow-origins '("https://example.com")))
         (req `(:method "GET"
                :headers (("origin" . "https://example.com"))))
         (res (evaluate-cors policy req)))

    (is (string= (header (cors-result-headers res)
                         "Access-Control-Allow-Origin")
                 "https://example.com"))
    (is (not (cors-result-handled-p res)))))

(test disallowed-origin
  (let* ((policy (make-cors-policy
                  :allow-origins '("https://good.com")))
         (req `(:method "GET"
                :headers (("origin" . "https://evil.com"))))
         (res (evaluate-cors policy req)))

    (is (null (cors-result-headers res)))))

(test valid-preflight
  (let* ((policy (make-cors-policy
                  :allow-origins '("*")
                  :allow-methods '("GET" "POST")))
         (req `(:method "OPTIONS"
                :headers (("origin" . "https://example.com")
                          ("access-control-request-method" . "POST"))))
         (res (evaluate-cors policy req)))

    (is (cors-result-handled-p res))
    (is (= 204 (cors-result-status res)))))

(test invalid-preflight-method
  (let* ((policy (make-cors-policy
                  :allow-origins '("*")
                  :allow-methods '("GET")))
         (req `(:method "OPTIONS"
                :headers (("origin" . "https://example.com")
                          ("access-control-request-method" . "DELETE"))))
         (res (evaluate-cors policy req)))

    (is (cors-result-handled-p res))
    (is (null (cors-result-headers res)))))

(test invalid-preflight-headers
  (let* ((policy (make-cors-policy
                  :allow-origins '("*")
                  :allow-headers '("X-Allowed")))
         (req `(:method "OPTIONS"
                :headers (("origin" . "https://example.com")
                          ("access-control-request-method" . "GET")
                          ("access-control-request-headers" . "X-Bad"))))
         (res (evaluate-cors policy req)))

    (is (cors-result-handled-p res))
    (is (null (cors-result-headers res)))))

(test options-without-preflight
  (let* ((policy (make-cors-policy))
         (req `(:method "OPTIONS"
                :headers (("origin" . "https://example.com"))))
         (res (evaluate-cors policy req)))

    (is (not (cors-result-handled-p res)))))

(test wildcard-origin
  (let* ((policy (make-cors-policy
                  :allow-origins '("*")))
         (req `(:method "GET"
                :headers (("origin" . "https://random.com"))))
         (res (evaluate-cors policy req)))

    (is (string=
         (header (cors-result-headers res)
                 "Access-Control-Allow-Origin")
         "https://random.com"))))

(test null-origin-allowed
  (let* ((policy (make-cors-policy
                  :allow-origins '("null")))
         (req `(:method "GET"
                :headers (("origin" . "null"))))
         (res (evaluate-cors policy req)))

    (is (string=
         (header (cors-result-headers res)
                 "Access-Control-Allow-Origin")
         "null"))))

(test null-origin-rejected
  (let* ((policy (make-cors-policy
                  :allow-origins '("https://example.com")))
         (req `(:method "GET"
                :headers (("origin" . "null"))))
         (res (evaluate-cors policy req)))

    (is (null (cors-result-headers res)))))

(test preflight-multiple-headers
  (let* ((policy (make-cors-policy
                  :allow-origins '("*")
                  :allow-headers '("X-A" "X-B")))
         (req `(:method "OPTIONS"
                :headers (("origin" . "https://example.com")
                          ("access-control-request-method" . "GET")
                          ("access-control-request-headers" . "X-A, X-B"))))
         (res (evaluate-cors policy req)))

    (is (cors-result-handled-p res))
    (is (= 204 (cors-result-status res)))))

(test preflight-partial-invalid-headers
  (let* ((policy (make-cors-policy
                  :allow-origins '("*")
                  :allow-headers '("X-A")))
         (req `(:method "OPTIONS"
                :headers (("origin" . "https://example.com")
                          ("access-control-request-method" . "GET")
                          ("access-control-request-headers" . "X-A, X-B"))))
         (res (evaluate-cors policy req)))

    (is (cors-result-handled-p res))
    (is (null (cors-result-headers res)))))

(test case-insensitive-headers
  (let* ((policy (make-cors-policy))
         (req `(:method "GET"
                 :headers (("Origin" . "https://example.com"))))
         (res (evaluate-cors policy req)))
    (is (string=
         (header (cors-result-headers res)
                 "Access-Control-Allow-Origin")
         "https://example.com"))))

(test no-origin-header
  (let* ((policy (make-cors-policy))
         (req `(:method "GET"
                :headers (("host" . "example.com"))))
         (res (evaluate-cors policy req)))

    (is (null (cors-result-headers res)))
    (is (not (cors-result-handled-p res)))))

(test preflight-contains-origin
  (let* ((policy (make-cors-policy
                  :allow-origins '("*")))
         (req `(:method "OPTIONS"
                :headers (("origin" . "https://example.com")
                          ("access-control-request-method" . "GET"))))
         (res (evaluate-cors policy req)))

    (is (string=
         (header (cors-result-headers res)
                 "Access-Control-Allow-Origin")
         "https://example.com"))))

(test simple-vary-header
  (let* ((policy (make-cors-policy))
         (req `(:method "GET"
                :headers (("origin" . "https://example.com"))))
         (res (evaluate-cors policy req)))

    (is (search "Origin"
                (header (cors-result-headers res) "Vary")))))

(test preflight-vary-header
  (let* ((policy (make-cors-policy))
         (req `(:method "OPTIONS"
                :headers (("origin" . "https://example.com")
                          ("access-control-request-method" . "GET")
                          ("access-control-request-headers" . "X-Test"))))
         (res (evaluate-cors policy req))
         (vary (header (cors-result-headers res) "Vary")))

    (is (search "Origin" vary))
    (is (search "Access-Control-Request-Method" vary))
    (is (search "Access-Control-Request-Headers" vary))))
