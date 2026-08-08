ifndef MK_DRIVERS_UDB
MK_DRIVERS_UDB := 1

include mk-drivers/toolchain.mk

###
# Points at the udb key-value store -- developed out-of-tree at
# github.com/Rewbert/nor-udb -- and builds it into an archive. NOR_UDB_DIR
# points at a checkout of that repo

NOR_UDB_DIR ?= ../nor-udb
NOR_UDB_SRC := $(wildcard $(NOR_UDB_DIR)/*.c)

ifeq ($(NOR_UDB_SRC),)
$(error udb sources not found under NOR_UDB_DIR=$(NOR_UDB_DIR) -- clone https://github.com/Rewbert/nor-udb and export NOR_UDB_DIR to point at it, e.g. export NOR_UDB_DIR=$$HOME/Projects/nor-udb)
endif

NOR_UDB_INC      := -I$(NOR_UDB_DIR)
NOR_UDB_CFLAGS   := $(CFLAGS) $(TRUSTZONE_AWARENESS) $(NO_STDLIB) $(NOSTARTFILES) $(DEBUG)
NOR_UDB_CPPFLAGS := $(CPPFLAGS) $(NOR_UDB_INC)

NOR_UDB_A := build/udb.a
NOR_UDB_O := $(patsubst $(NOR_UDB_DIR)/%.c,build/nor-udb/%.o,$(NOR_UDB_SRC))

build/nor-udb/%.o: $(NOR_UDB_DIR)/%.c
	mkdir -p $(dir $@)
	$(CC) $(NOR_UDB_CFLAGS) $(NOR_UDB_CPPFLAGS) -c $< -o $@

$(NOR_UDB_A): $(NOR_UDB_O)
	$(AR) rcs $@ $^

endif
