ENV?=development

## run through roswell
LISP?=sbcl

LISPFLAGS=--quit --non-interactive

.PHONY: tests
tests:
	ENV=$(ENV) \
	$(LISP) \
	$(LISPFLAGS) --load tests-runner.lisp
