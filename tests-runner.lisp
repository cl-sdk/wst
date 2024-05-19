(push *default-pathname-defaults* ql:*local-project-directories*)

(quicklisp:quickload :wst.routing.test)

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore c h))
        (uiop:quit -1))
      fiveam:*on-error* :debug)

(unless (fiveam:run-all-tests)
  (exit :code 1 :abort t))
