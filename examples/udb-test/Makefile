include mk-drivers/board.mk
include mk-drivers/udb.mk

###
# Build the udb-test example: a secure/non-secure pair where each world runs its
# own udb (udb.h, out-of-tree -- see NOR_UDB_DIR in mk-drivers/udb.mk) database,
# backed by the last erase pages of its own flash.
# The shared/ driver files are compiled into both worlds; $(BOARD) selects the
# stm32l5-drv.c / stm32u5-drv.c backend.
#
# Build from the repo root (name the 'all' target; the bare default goal
# resolves to the shared driver archive):
#   make -f examples/udb-test/Makefile all                  # stm32l5 (default)
#   make -f examples/udb-test/Makefile all BOARD=stm32u5    # stm32u5
###

APP := examples/udb-test

EXAMPLE_INC := -I$(APP)/shared $(NOR_UDB_INC)

SECURE_CPPFLAGS    += $(EXAMPLE_INC)
NONSECURE_CPPFLAGS += $(EXAMPLE_INC)

# The two worlds each run their own database and never call across the secure
# boundary, so there are no non-secure-callable veneers and no CMSE import
# library to share (unlike the other TrustZone examples).

# --- Secure application ---

SECURE_SRC := \
  $(APP)/S/main.c \
  $(APP)/shared/flash-drv.c \
  $(APP)/shared/$(BOARD)-drv.c

SECURE_O := $(patsubst %.c, build/s/%.o, $(SECURE_SRC))

build/s/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

# --- Non-secure application ---

NONSECURE_SRC := \
  $(APP)/NS/main.c \
  $(APP)/shared/flash-drv.c \
  $(APP)/shared/$(BOARD)-drv.c

NONSECURE_O := $(patsubst %.c, build/ns/%.o, $(NONSECURE_SRC))

build/ns/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

# --- Link targets ---

.PHONY: all
all: secure.elf nonsecure.elf

secure.elf: $(SECURE_BOOT_O) $(BOARD_O) $(SECURE_O) $(TZ_LIB_DRV_SECURE_A) $(NOR_UDB_A)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^

nonsecure.elf: $(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NONSECURE_O) $(TZ_LIB_DRV_NONSECURE_A) $(NOR_UDB_A)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ $^
