### Makefile for Yasnippet (stolen from Eglot)
# Variables
#
EMACS?=emacs
SELECTOR?=t
ERROR_ON_WARN=nil

LOAD_PATH=-L .

ELFILES := yasnippet.el yasnippet-tests.el
ELCFILES := $(ELFILES:.el=.elc)

BYTECOMP_ERROR_ON_WARN := \
	--eval '(setq byte-compile-error-on-warn $(ERROR_ON_WARN))'

all: compile

%.elc: %.el
	$(EMACS) -Q $(BYTECOMP_ERROR_ON_WARN) $(LOAD_PATH) \
		--batch -f batch-byte-compile $<

compile: $(ELCFILES)

# Automated tests
#
yasnippet-check: compile
	$(EMACS) -Q --batch						\
		$(LOAD_PATH)						\
		-l yasnippet						\
		-l yasnippet-tests						\
		--eval '(setq ert-batch-backtrace-right-margin 200)'	\
		--eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))'

interactive: compile
	$(EMACS) -Q							\
		$(LOAD_PATH)						\
		-l yasnippet						\
		-l yasnippet-tests						\

check: yasnippet-check

# Cleanup
#
clean:
	find . -iname '*.elc' -exec rm {} \;
.PHONY: all compile clean check
