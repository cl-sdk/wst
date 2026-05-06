ENV?=development

## run through roswell
LISP?=sbcl --sysinit ./.sbclrc

LISPFLAGS=--non-interactive

.PHONY: tests
tests:
	ENV=$(ENV) \
	$(LISP) \
	$(LISPFLAGS) --quit --load tests-runner.lisp
