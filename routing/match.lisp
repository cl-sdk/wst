(in-package :wst.routing)

(declaim (ftype (function (matcher symbol list integer) list)
                match))
(defun match (matcher method segments count)
  "Run the MATCHER for METHOD, SEGMENTS and COUNT."
  (if (or (not (= count (matcher-segments-count matcher)))
         (not (equal method (matcher-method matcher))))
      (list :skip nil)
      (list :params (loop :for x :in (matcher-segments matcher)
                          :for y :in segments
                          :if (str:starts-with? ":" x)
                            :collect (cons (str:substring 1 (length x) x) y)
                          :else :if (not (equal x y))
                                  :do (return-from match (list :skip nil))))))

(declaim (ftype (function (route symbol list integer) list)
                do-matcher))
(defun do-matcher (route method segments count)
  (destructuring-bind (action params)
      (match (route-matcher route) method segments count)
    (when (equal action :params)
      (cons route params))))

(defun match-route (path method &optional (routes *routes*))
  "Find a route by PATH and METHOD."
  (let* ((segments
           (remove-if (lambda (p) (or (null p) (= 0 (length p))))
                      (cdr (str:split "/" path))))
         (count (length segments)))
    (loop :for route :in routes
          :do (alexandria:when-let ((match-data (do-matcher route method segments count)))
                (return match-data)))))
