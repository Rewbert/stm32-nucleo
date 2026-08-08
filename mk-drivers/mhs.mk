ifndef MK_DRIVERS_MHS
MK_DRIVERS_MHS := 1

ifeq ($(origin MHS_ROOT),undefined)
$(error MHS_ROOT is not set -- clone https://github.com/Rewbert/MicroHs and export MHS_ROOT to point at it, e.g. export MHS_ROOT=$$HOME/MHS_DIR/MicroHs)
endif

MHS_DIR := $(MHS_ROOT)/src/runtime
MHS_BIN := $(MHS_ROOT)/bin/mhs

ifeq ($(wildcard $(MHS_BIN)),)
$(error MicroHs not found under MHS_ROOT=$(MHS_ROOT) -- check that it points at a valid MicroHs checkout)
endif

endif
