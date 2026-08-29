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

# Every example here is meant to fail to typecheck, so mhs's own -C never gets
# a chance to save its cache -- that only happens after a successful top-level
# compile (confirmed: a failing compile leaves no .mhscache at all). So instead
# we seed the cache once by compiling MicroHasTEE.hs, the library aggregator
# every example here imports (directly or via Effectful.Setup/HAL/NonSecure) --
# it's a real module with no CPP-gated content and no `main` requirement, so it
# compiles clean and mhs happily writes the cache. Every reject example is then
# read-only (-CR) against it, so it only has to freshly typecheck its own small
# file instead of the whole library each time.
#
# CACHE is a real file target depending on the library sources, not a "seed
# once" heuristic -- a stale cache could silently mask a real regression here,
# and this suite exists specifically to catch the type system failing to
# reject something (see header comment above), so a false pass is unacceptable.
#
# Don't run this concurrently with the top-level examples/microhastee/Makefile
# build (make / make simple) -- both swap files into the same hardcoded
# $(HASKELL_DIR)/.mhscache and would race.
CACHE       := .reject-cache/.mhscache
LIB_SRC     := $(wildcard $(HASKELL_DIR)/src/Control/*/*.hs $(HASKELL_DIR)/src/Control/*/*/*.hs \
                           $(HASKELL_DIR)/src/Foreign/*/*.hs \
                           $(HASKELL_DIR)/src/Effectful/*.hs $(HASKELL_DIR)/src/Effectful/*/*.hs \
                           $(HASKELL_DIR)/src/HAL.hs $(HASKELL_DIR)/src/HAL/*.hs $(HASKELL_DIR)/src/HAL/*/*.hs) \
               $(HASKELL_DIR)/src/MicroHasTEE.hs

.PHONY: all check show clean

all: check

$(CACHE): $(LIB_SRC)
	mkdir -p $(dir $(CACHE))
	rm -f $(HASKELL_DIR)/.mhscache
	cd $(HASKELL_DIR) && $(MHS_BIN) -CW -isrc -iapp -c -o.reject-seed.c src/MicroHasTEE.hs
	cp -f $(HASKELL_DIR)/.mhscache $(CACHE)
	rm -f $(HASKELL_DIR)/.mhscache $(HASKELL_DIR)/.reject-seed.c

# Compile every example and assert mhs rejects each one.
check: $(CACHE)
	@mkdir -p $(LOGDIR)
	@cp -f $(CACHE) $(HASKELL_DIR)/.mhscache
	@total=0; failed=0; \
	for f in $(SOURCES); do \
	  total=$$((total+1)); \
	  log=$(LOGDIR)/$$f.log; \
	  if (cd $(HASKELL_DIR) && $(MHS_BIN) -CR -isrc -iapp -c -o$(SCRATCH) rejects/$$f) >$$log 2>&1; then \
	    echo "FAIL (incorrectly ACCEPTED): $$f"; \
	    failed=$$((failed+1)); \
	  else \
	    echo "ok   (correctly rejected):  $$f"; \
	  fi; \
	done; \
	rm -f $(HASKELL_DIR)/$(SCRATCH) $(HASKELL_DIR)/.mhscache; \
	echo "----"; \
	echo "$$((total-failed))/$$total examples correctly rejected"; \
	if [ $$failed -gt 0 ]; then \
	  echo "$$failed example(s) were incorrectly ACCEPTED -- see above and $(LOGDIR)/"; \
	  exit 1; \
	fi

# Compile every example and print the actual mhs error for each -- useful
# for pulling quotes/line numbers into the paper.
show: $(CACHE)
	@cp -f $(CACHE) $(HASKELL_DIR)/.mhscache
	@for f in $(SOURCES); do \
	  echo "=== $$f ==="; \
	  (cd $(HASKELL_DIR) && $(MHS_BIN) -CR -isrc -iapp -c -o$(SCRATCH) rejects/$$f) 2>&1 | tail -n +1; \
	  echo; \
	done; \
	rm -f $(HASKELL_DIR)/$(SCRATCH) $(HASKELL_DIR)/.mhscache

clean:
	rm -rf $(LOGDIR) $(HASKELL_DIR)/$(SCRATCH) .reject-cache
