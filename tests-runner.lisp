(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))

(push *default-pathname-defaults* ql:*local-project-directories*)

(asdf:oos 'asdf:load-op :wst.routing.test :force t)

(defun run-tests (coverage)
  (5am:run-all-tests)
  (when coverage
    (sb-cover:report #P"./coverage/")))

(setf *debugger-hook*
      (lambda (c h)
	(declare (ignore c h))
	(uiop:quit -1))
      fiveam:*on-error* nil)

(unless (run-tests t)
  (exit :code 1 :abort t))
