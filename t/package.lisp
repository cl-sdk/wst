(defpackage #:io.github.cl-sdk.wst.test
  (:use #:cl))

(in-package :io.github.cl-sdk.wst.test)

(defmacro def-route-testing (name args &body body)
  "Define a FiveAM test that resets all global routing state after running."
  (declare (ignorable args))
  `(5am:def-test ,name ()
     ,@body
     (setf io.github.cl-sdk.wst.routing::*routes* nil
           io.github.cl-sdk.wst.routing::*condition-handler* nil
           io.github.cl-sdk.wst.routing::*any-route-handler* nil)))
