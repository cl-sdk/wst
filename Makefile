ENV?=development

## run through roswell
LISP?=sbcl

LISPFLAGS=--non-interactive

.PHONY: tests
tests:
	ENV=$(ENV) \
	$(LISP) \
	$(LISPFLAGS) --quit --load tests-runner.lisp

.PHONY: run-url-shortener
run-url-shortener:
	ENV=$(ENV) \
	$(LISP) \
	$(LISPFLAGS) \
	--eval "(push *default-pathname-defaults* ql:*local-project-directories*)" \
	--eval "(ql:quickload :wst.example.url-shortener)"

.PHONY: run-bookmark-manager
run-bookmark-manager:
	ENV=$(ENV) \
	$(LISP) \
	$(LISPFLAGS) \
	--eval "(push *default-pathname-defaults* ql:*local-project-directories*)" \
	--eval "(ql:quickload :wst.example.bookmark-manager)"
