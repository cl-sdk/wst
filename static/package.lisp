(defpackage #:wst.routing.static
  (:use #:cl)
  (:export
   #:change-static-path
   #:route-static))

(in-package :wst.routing.static)

(defparameter *static-path* *default-pathname-defaults*
  "Pathname specifying the directory where all static files are located.")

(defun change-static-path (path)
  "Sets the directory path for serving static files to PATH."
  (setf *static-path* path))

(defmacro route-static (name path mime)
  "Defines a static file route handler.

Arguments:
  - NAME: The symbol to name the generated handler function.
  - PATH: The URL path at which the static file will be served.
  - MIME: The MIME type string to set in the response's Content-Type header.

The macro creates a function named NAME that reads the file at the static
directory concatenated with PATH, then writes it to the RESPONSE with status 200
and the specified MIME type. It also removes any existing route with the same
name and registers the new route for HTTP GET requests at PATH."
  `(progn
     (remove-route ',name)
     (defun ,name (request response)
       (declare (ignorable request response))
       (let* ((serving (concatenate 'string (namestring ,*static-path*) ,path))
              (content (read-file-string serving)))
         (write-response response
                         :status 200
                         :content-type ,mime
                         :content content)))
     (add-route ',name ,path :get #',name)))
