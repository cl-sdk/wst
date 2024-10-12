(in-package :wst.routing)

(defstruct request
  "Request object."
  (uri "" :type string)
  (query "" :type string)
  (hash "" :type string)
  (method :GET :type symbol)
  (headers (hash-create nil) :type hash-table)
  (content-type nil :type (or string null))
  (content-length 0 :type integer)
  content
  (data nil :type list))

(defstruct response
  "Response object."
  (status 0 :type integer)
  (headers nil :type list)
  (content "" :type string)
  (data nil :type list))

(defvar *routes* nil
  "Hash to hold all routes.")

(defmacro with-request-data (keys request &body body)
  (let ((ref (gensym "DATA")))
    `(let* ((,ref (request-data ,request))
            ,@(mapcar (lambda (item)
                        (list (intern (string-upcase (string item)))
                              `(getf ,ref ,(intern (string item) :keyword))))
                      keys))
       ,@body)))

(defmacro with-response-data (keys response &body body)
  (let ((ref (gensym "DATA")))
    `(let* ((,ref (response-data ,response))
            ,@(mapcar (lambda (item)
                        (list (intern (string-upcase (string item)))
                              `(getf ,ref ,(intern (string item) :keyword))))
                      keys))
       ,@body)))

(defmacro with-request-params (keys params &body body)
  (let ((ref (gensym "PARAMS")))
    `(let* ((,ref ,params)
            ,@(mapcar (lambda (item)
                        (etypecase item
                          (symbol (list (intern (string item))
                                        `(alexandria:assoc-value ,ref
                                                                 ,(string-downcase (string item))
                                                                 :test
                                                                 #'string-equal)))
                          (cons (list (intern (string (car item)))
                                      `(let ((value (alexandria:assoc-value ,ref
                                                                            ,(string-downcase (string (car item)))
                                                                            :test
                                                                            #'string-equal)))
                                         (if value (funcall ,(cdr item) value) value))))))
                      keys))
       ,@body)))
