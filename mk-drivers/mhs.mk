ifndef MK_DRIVERS_MHS
MK_DRIVERS_MHS := 1

MHS_ROOT ?= $(HOME)/MHS_DIR/MicroHs
MHS_DIR  := $(MHS_ROOT)/src/runtime
MHS_BIN  := $(MHS_ROOT)/bin/mhs

ifeq ($(wildcard $(MHS_BIN)),)
$(error MicroHs not found under MHS_ROOT=$(MHS_ROOT) -- clone https://github.com/Rewbert/MicroHs and export MHS_ROOT to point at it, e.g. export MHS_ROOT=$$HOME/MHS_DIR/MicroHs)
endif

endif
