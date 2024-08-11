(in-package :wst.routing)

(defparameter *static-path* *default-pathname-defaults*
  "Path to find all static files.")

(defun change-static-path (path)
  "Change static files path to PATH."
  (setf *static-path* path))

(defmacro route-static (name path mime)
  "Define a route for a static file with NAME for the function's name,
 PATH to be requested and MIME type."
  `(progn
     (remove-route ',name)
     (defun ,name (request response)
       (declare (ignorable request response))
       (let* ((serving (concatenate 'string (namestring ,*static-path*) ,path))
	      (content (read-file-string serving)))
	 (log:info serving content)
	 (write-response response
			 :status 200
			 :content-type ,mime
			 :content content)))
     (add-route ',name ,path :get #',name)))
