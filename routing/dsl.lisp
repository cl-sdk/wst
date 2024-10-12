(defpackage #:wst.routing.dsl
  (:use #:cl)
  (:import-from #:cl-hash-util
                #:hash
                #:with-keys)
  (:import-from #:wst.routing
                #:any-route-handler)
  (:import-from #:alexandria
                #:ensure-list)
  (:import-from #:wst.routing
                #:add-route)
  (:import-from #:wst.routing
                #:remove-route)
  (:import-from #:str
                #:join)
  (:export
   #:build-webserver
   #:wrap
   #:any-route
   #:route
   #:group
   #:resource))

(in-package #:wst.routing.dsl)

(defun %any-route (api stack)
  (with-keys ((bfs "befores") (paths "paths") (afs "afters"))
      stack
    (destructuring-bind (method &rest rest)
        api
      (let ((actions (append (reduce #'append bfs)
                             rest
                             (reduce #'append afs))))
        (any-route-handler
         method
         (lambda (request response)
           (loop :for fn :in actions
                 :do (funcall fn request response)
                 :finally (return response))))))))

(defun %create-route (api stack)
  "Execute `route` is found take the API and build with the STACK."
  (with-keys ((bfs "befores") (paths "paths") (afs "afters"))
      stack
    (destructuring-bind (method route-name path &rest rest)
        api
      (let* ((onstack (join "" paths))
             (route-path (if (stringp path) path ""))
             (the-path (concatenate 'string onstack route-path))
             (the-action (if (not (stringp path)) path (car rest)))
             (actions (append
                       (reduce #'append bfs)
                       (ensure-list the-action)
                       (reduce #'append afs))))
        (remove-route route-name)
        (add-route route-name the-path method
                   (lambda (request response)
                     (loop :for fn :in actions
                           :do (funcall fn request response)
                           :finally (return response))))))))

(defun %wrap-routes (api stack)
  "Execute when `wrap is found.
 The API can have keys:

 - :before (list) - run a sequence of functions before the route.
 - :route  (list) - the actual route or routing group.
 - :after  (list) - run a sequence of functions after the route."
  (with-keys ((bfs "befores") (afs "afters"))
      stack
    (progn
      (setf bfs (append bfs (list (ensure-list (getf api :before nil))))
            afs (append afs (list (ensure-list (getf api :after nil)))))
      (%build-webserver (getf api :route) stack)
      (setf bfs (butlast bfs)
            afs (butlast afs)))))

(defun %build-webserver (api stack)
  "Build from a API definition"
  (let ((item (car api))
        (routes (cdr api)))
    (case item
      (wrap (%wrap-routes routes stack))
      (any-route (%any-route routes stack))
      (route (%create-route routes stack))
      (group (map nil (lambda (api) (%build-webserver api stack)) routes))
      (resource (destructuring-bind (path &rest rest)
                    (cdr api)
                  (setf (gethash "paths" stack)
                        (append (gethash "paths" stack) (list path)))
                  (map nil (lambda (item) (%build-webserver item stack)) rest)
                  (setf (gethash "paths" stack)
                        (butlast (gethash "paths" stack))))))))

(defun build-webserver (api)
  "Build web server from a API definition."
  (%build-webserver api (hash ("paths"  nil) ("befores"  nil) ("afters"  nil))))
