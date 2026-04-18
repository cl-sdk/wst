(defpackage #:wst.routing.test
  (:use #:cl))

(in-package :wst.routing.test)

(defmacro def-route-testing (name args &body body)
  "Define a FiveAM test that resets all global routing state after running."
  (declare (ignorable args))
  `(5am:def-test ,name ()
     ,@body
     (setf wst.routing::*routes* nil
           wst.routing::*condition-handler* nil
           wst.routing::*any-route-handler* nil)))
