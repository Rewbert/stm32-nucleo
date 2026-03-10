include mk-drivers/board.mk

TZ_DRV_EXAMPLE     := TZ-drivers-example
TZ_DRV_EXAMPLE_INC := -I$(TZ_DRV_EXAMPLE)/shared

# Shared breadboard code — compiled for both domains
SHARED_SRC := $(TZ_DRV_EXAMPLE)/shared/breadboard.c
SHARED_S_O := build/s/$(TZ_DRV_EXAMPLE)/shared/breadboard.o
SHARED_NS_O := build/ns/$(TZ_DRV_EXAMPLE)/shared/breadboard.o

build/s/$(TZ_DRV_EXAMPLE)/shared/%.o: $(TZ_DRV_EXAMPLE)/shared/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(TZ_DRV_EXAMPLE_INC) -c $< -o $@

build/ns/$(TZ_DRV_EXAMPLE)/shared/%.o: $(TZ_DRV_EXAMPLE)/shared/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(TZ_DRV_EXAMPLE_INC) -c $< -o $@

# Secure application
S_APP_SRC := $(TZ_DRV_EXAMPLE)/S/main.c
S_APP_O   := build/s/$(TZ_DRV_EXAMPLE)/S/main.o

build/s/$(TZ_DRV_EXAMPLE)/S/%.o: $(TZ_DRV_EXAMPLE)/S/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(TZ_DRV_EXAMPLE_INC) -c $< -o $@

# Nonsecure application
NS_APP_SRC := $(TZ_DRV_EXAMPLE)/NS/main.c
NS_APP_O   := build/ns/$(TZ_DRV_EXAMPLE)/NS/main.o

build/ns/$(TZ_DRV_EXAMPLE)/NS/%.o: $(TZ_DRV_EXAMPLE)/NS/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(TZ_DRV_EXAMPLE_INC) -c $< -o $@

SECURE_LIB          := build/tz-drivers-secure_cmse_import.lib
SECURE_IMPLIB_FLAGS := -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

.PHONY: all
all: tz-secure.elf tz-nonsecure.elf

$(dir $(SECURE_LIB)):
	mkdir -p $@

tz-secure.elf: $(SECURE_BOOT_O) $(BOARD_O) $(SHARED_S_O) $(S_APP_O) $(TZ_LIB_DRV_SECURE_A) | $(dir $(SECURE_LIB))
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(SECURE_IMPLIB_FLAGS)

tz-nonsecure.elf: tz-secure.elf $(NONSECURE_BOOT_O) $(BOARD_NS_O) $(SHARED_NS_O) $(NS_APP_O) $(TZ_LIB_DRV_NONSECURE_A)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ \
		$(NONSECURE_BOOT_O) $(BOARD_NS_O) $(SHARED_NS_O) $(NS_APP_O) $(TZ_LIB_DRV_NONSECURE_A) $(SECURE_LIB)
