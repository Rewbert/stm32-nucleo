include mk-drivers/board.mk

###
# Build the blink-and-button example: a small secure/non-secure pair using the
# TZ-lib-drivers/boards framework.
###

APP := examples/blink-and-button

SECURE_LIB          := build/$(APP)/secure_cmse_import.lib
SECURE_IMPLIB_FLAGS := -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

# --- Secure application ---

SECURE_SRC := $(APP)/S/main.c
SECURE_O   := $(patsubst %.c, build/s/%.o, $(SECURE_SRC))

build/s/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

# --- Non-secure application ---

NONSECURE_SRC := $(APP)/NS/main.c
NONSECURE_O   := $(patsubst %.c, build/ns/%.o, $(NONSECURE_SRC))

build/ns/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

# --- Link targets ---

.PHONY: all
all: secure.elf nonsecure.elf

$(dir $(SECURE_LIB)):
	mkdir -p $@

secure.elf: $(SECURE_BOOT_O) $(BOARD_O) $(SECURE_O) $(TZ_LIB_DRV_SECURE_A) | $(dir $(SECURE_LIB))
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(SECURE_IMPLIB_FLAGS)

nonsecure.elf: secure.elf $(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NONSECURE_O) $(TZ_LIB_DRV_NONSECURE_A)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ \
		$(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NONSECURE_O) $(TZ_LIB_DRV_NONSECURE_A) $(SECURE_LIB)
