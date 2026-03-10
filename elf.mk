include mk-drivers/board.mk

APP_SRC := main-drivers/main.c
APP_O   := build/s/main-drivers/main.o

SECURE_LIB          := build/secure_cmse_import.lib
SECURE_IMPLIB_FLAGS := -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

build/s/main-drivers/%.o: main-drivers/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

NS_APP_SRC := ns-main-drivers/main.c
NS_APP_O   := build/ns/ns-main-drivers/main.o

build/ns/ns-main-drivers/%.o: ns-main-drivers/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

.PHONY: all
all: secure.elf nonsecure.elf

$(dir $(SECURE_LIB)):
	mkdir -p $@

secure.elf: $(SECURE_BOOT_O) $(BOARD_O) $(APP_O) $(TZ_LIB_DRV_SECURE_A) | $(dir $(SECURE_LIB))
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(SECURE_IMPLIB_FLAGS)

nonsecure.elf: secure.elf $(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NS_APP_O) $(TZ_LIB_DRV_NONSECURE_A)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ \
		$(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NS_APP_O) $(TZ_LIB_DRV_NONSECURE_A) $(SECURE_LIB)
