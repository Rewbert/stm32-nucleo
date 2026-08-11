# STM32U5 only -- matches the udb PERSIST-region addresses hardcoded in
# shared/udb-drv.c (copied from examples/microhastee) and this project's
# board primacy. Must be set before the includes below, since
# mk-drivers/toolchain.mk only defaults BOARD if it isn't already set.
BOARD := stm32u5

include mk-drivers/board.mk
include mk-drivers/udb.mk

###
# Build the door-lock-c example: a hand-written C baseline for
# examples/microhastee's Haskell DoorLock.hs (see S/main.c's header comment
# for the design and the comparison it's meant to support). Same trust split
# and board wiring: secure owns the PIN/lockout counter/door-state LEDs,
# non-secure owns the keypad (4 GPIOs, EXTI-driven) and the console.
#
# Build from the repo root (the 'all' target must be named):
#   make -f examples/door-lock-c/Makefile all
###

APP := examples/door-lock-c

EXAMPLE_INC := -I$(APP)/shared $(NOR_UDB_INC)

SECURE_CPPFLAGS    += $(EXAMPLE_INC)
NONSECURE_CPPFLAGS += $(EXAMPLE_INC)

SECURE_LIB          := build/$(APP)/secure_cmse_import.lib
SECURE_IMPLIB_FLAGS := -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

# --- Secure application ---

SECURE_SRC := \
  $(APP)/S/main.c \
  $(APP)/shared/keypad.c \
  $(APP)/shared/flash-drv.c \
  $(APP)/shared/udb-drv.c

SECURE_O := $(patsubst %.c, build/s/%.o, $(SECURE_SRC))

build/s/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

# --- Non-secure application ---

NONSECURE_SRC := \
  $(APP)/NS/main.c \
  $(APP)/shared/keypad.c

NONSECURE_O := $(patsubst %.c, build/ns/%.o, $(NONSECURE_SRC))

build/ns/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

# --- Link targets ---

.PHONY: all
all: secure.elf nonsecure.elf

$(dir $(SECURE_LIB)):
	mkdir -p $@

secure.elf: $(SECURE_BOOT_O) $(BOARD_O) $(SECURE_O) $(TZ_LIB_DRV_SECURE_A) $(NOR_UDB_A) | $(dir $(SECURE_LIB))
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(SECURE_IMPLIB_FLAGS)

nonsecure.elf: secure.elf $(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NONSECURE_O) $(TZ_LIB_DRV_NONSECURE_A) $(NOR_UDB_A)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ \
		$(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NONSECURE_O) $(TZ_LIB_DRV_NONSECURE_A) $(NOR_UDB_A) $(SECURE_LIB)
