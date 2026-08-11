###
# Negative test suite for the Effectful.Setup capability system.
#
# Every .hs file in this directory is a deliberately ILL-FORMED program: each
# one violates exactly one invariant that Effectful.Setup's type-level
# ledger (Member/Delete/Fresh over Cons-lists, plus the Locked/Unlocked phase
# gate) is supposed to enforce -- fetching the same peripheral twice,
# reusing a handle after release, a Secure/Nonsecure action touching a
# peripheral it was never given, configuring after lock_configuration, etc.
#
# `make check` compiles each one and asserts that mhs REJECTS it. A file that
# compiles clean is a test FAILURE -- it means the type system failed to
# catch something it's supposed to catch, not a passing test. This is the
# source for the rejection gallery in paper/main.tex.
#
# Note: mhs's reported error line does not always point at the exact
# offending statement (see e.g. RejectDoubleRelease.hs, where it points at
# the first 'gpio_release' rather than the second) -- this is a known
# MicroHs constraint-solver quirk, not a sign the wrong thing was rejected.
# What matters is that the program as a whole fails to compile.
###

ifeq ($(origin MHS_ROOT),undefined)
$(error MHS_ROOT is not set -- see ../../../mk-drivers/mhs.mk)
endif

MHS_BIN     := $(MHS_ROOT)/bin/mhs
HASKELL_DIR := ..
SCRATCH     := rejects/.reject-scratch.c
SOURCES     := $(wildcard *.hs)
LOGDIR      := .logs

.PHONY: all check show clean

all: check

# Compile every example and assert mhs rejects each one.
check:
	@mkdir -p $(LOGDIR)
	@total=0; failed=0; \
	for f in $(SOURCES); do \
	  total=$$((total+1)); \
	  log=$(LOGDIR)/$$f.log; \
	  if (cd $(HASKELL_DIR) && $(MHS_BIN) -ilib -iapp -c -o$(SCRATCH) rejects/$$f) >$$log 2>&1; then \
	    echo "FAIL (incorrectly ACCEPTED): $$f"; \
	    failed=$$((failed+1)); \
	  else \
	    echo "ok   (correctly rejected):  $$f"; \
	  fi; \
	done; \
	rm -f $(HASKELL_DIR)/$(SCRATCH); \
	echo "----"; \
	echo "$$((total-failed))/$$total examples correctly rejected"; \
	if [ $$failed -gt 0 ]; then \
	  echo "$$failed example(s) were incorrectly ACCEPTED -- see above and $(LOGDIR)/"; \
	  exit 1; \
	fi

# Compile every example and print the actual mhs error for each -- useful
# for pulling quotes/line numbers into the paper.
show:
	@for f in $(SOURCES); do \
	  echo "=== $$f ==="; \
	  (cd $(HASKELL_DIR) && $(MHS_BIN) -ilib -iapp -c -o$(SCRATCH) rejects/$$f) 2>&1 | tail -n +1; \
	  echo; \
	done; \
	rm -f $(HASKELL_DIR)/$(SCRATCH)

clean:
	rm -rf $(LOGDIR) $(HASKELL_DIR)/$(SCRATCH)
