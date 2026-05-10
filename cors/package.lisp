(defpackage :io.github.cl-sdk.wst.cors
  (:use :cl)
  (:export
   :make-cors-policy
   :evaluate-cors
   :cors-result
   :cors-result-headers
   :cors-result-status
   :cors-result-body
   :cors-result-handled-p))

(in-package :io.github.cl-sdk.wst.cors)

(defstruct cors-policy
  (allow-origins '("*"))
  (allow-methods '("GET" "POST" "PUT" "DELETE" "OPTIONS"))
  (allow-headers '("*"))
  (allow-credentials nil)
  (max-age 86400))

(defstruct cors-result
  headers
  status
  body
  handled-p)

(defun normalize-header-name (name)
  (string-downcase name))

(defun header-value (headers name)
  (cdr (or (assoc (string-downcase name)
		 headers
		 :test #'string=)
	  (assoc name
		 headers
		 :test #'string=))))

(defun join (list)
  (format nil "~{~A~^, ~}" list))

(defun split-header-list (value)
  (when value
    (let ((result nil)
	  (start 0))
      (loop for i from 0 to (length value)
	    do (when (or (= i (length value))
			(char= (char value i) #\,))
		 (let ((part (string-trim '(#\Space #\Tab)
					  (subseq value start i))))
		   (push part result))
		 (setf start (1+ i))))
      (nreverse result))))

(defun add-header (headers name value)
  (acons name value headers))

(defun cors-request-p (headers)
  (header-value headers "origin"))

(defun preflight-request-p (method headers)
  (and (string-equal method "OPTIONS")
     (header-value headers "access-control-request-method")))

(defun validate-policy (policy)
  (when (and (cors-policy-allow-credentials policy)
	   (member "*" (cors-policy-allow-origins policy)
		   :test #'string=))
    (error "Invalid CORS policy: wildcard origin cannot be used with credentials")))

(defun origin-allowed-p (origin policy)
  (let ((allowed (cors-policy-allow-origins policy)))
    (cond
      ((string= origin "null")
       (member "null" allowed :test #'string=))
      ((member "*" allowed :test #'string=)
       t)
      (t
       (member origin allowed :test #'string=)))))

(defun validate-preflight (policy headers)
  (let* ((req-method (header-value headers "access-control-request-method"))
	 (req-headers (split-header-list
		       (header-value headers "access-control-request-headers"))))

    ;; Method must be allowed
    (unless (member req-method
		    (cors-policy-allow-methods policy)
		    :test #'string=)
      (return-from validate-preflight nil))

    ;; Headers must be allowed
    (when req-headers
      (unless (every (lambda (h)
		       (or (member "*" (cors-policy-allow-headers policy)
				  :test #'string=)
			  (member h (cors-policy-allow-headers policy)
				  :test #'string=)))
		     req-headers)
	(return-from validate-preflight nil)))

    t))

(defun build-simple-response (policy origin)
  (let ((headers nil))
    (setf headers (add-header headers "Access-Control-Allow-Origin" origin))

    (when (cors-policy-allow-credentials policy)
      (setf headers
	    (add-header headers "Access-Control-Allow-Credentials" "true")))

    (setf headers (add-header headers "Vary" "Origin"))

    headers))

(defun build-preflight-response (policy origin)
  (let ((headers nil))

    (setf headers (add-header headers "Access-Control-Allow-Origin" origin))

    (setf headers
	  (add-header headers "Access-Control-Allow-Methods"
		      (join (cors-policy-allow-methods policy))))

    (setf headers
	  (add-header headers "Access-Control-Allow-Headers"
		      (join (cors-policy-allow-headers policy))))

    (setf headers
	  (add-header headers "Access-Control-Max-Age"
		      (write-to-string (cors-policy-max-age policy))))

    (when (cors-policy-allow-credentials policy)
      (setf headers
	    (add-header headers "Access-Control-Allow-Credentials" "true")))

    (setf headers
	  (add-header headers "Vary"
		      "Origin, Access-Control-Request-Method, Access-Control-Request-Headers"))

    headers))

(defun evaluate-cors (policy request)
  (validate-policy policy)

  (let* ((method (getf request :method))
	 (headers (getf request :headers))
	 (origin (header-value headers "Origin")))

    ;; Not a CORS request
    (unless origin
      (return-from evaluate-cors
	(make-cors-result :headers nil :handled-p nil)))

    ;; Origin not allowed
    (unless (origin-allowed-p origin policy)
      (return-from evaluate-cors
	(make-cors-result :headers nil :handled-p nil)))

    ;; Preflight
    (if (preflight-request-p method headers)

	(if (validate-preflight policy headers)
	    (make-cors-result
	     :headers (build-preflight-response policy origin)
	     :status 204
	     :body ""
	     :handled-p t)

	    ;; Reject
	    (make-cors-result :headers nil :handled-p t))

	;; Simple
	(make-cors-result
	 :headers (build-simple-response policy origin)
	 :handled-p nil))))
