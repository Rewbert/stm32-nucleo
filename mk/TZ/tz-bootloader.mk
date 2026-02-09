include mk/TZ/tz-common.mk

# The 'bootloader' is slightly more than a bootloader, but not much. Right now, hard-wired into every TZ project, is
# the linker script. This script defines the boundary between secure and nonsecure world, and the sizes of those
# segments. To my understanding, the bootloader is otherwise 'just' the reset handler and all that crap?
# TODO: Ask Joel what the definition is, or if my instinct is correct.

TZ_BOOTLOADER = TZ-bootloader

SECURE_BOOTLOADER_C = \
  $(TZ_BOOTLOADER)/S/src/bootloader.c \
  $(TZ_BOOTLOADER)/S/src/security_config.c

NONSECURE_BOOTLOADER_C = $(TZ_BOOTLOADER)/NS/src/bootloader.c

SECURE_BOOTLOADER_O = $(patsubst %.c, build/bootloader/%.o, $(SECURE_BOOTLOADER_C))
NONSECURE_BOOTLOADER_O = $(patsubst %.c, build/bootloader/%.o, $(NONSECURE_BOOTLOADER_C))

BOOTLOADER_INCLUDE = -I$()

SECURE_LINKER_FILE = $(TZ_BOOTLOADER)/S/ls-s.ld
NONSECURE_LINKER_FILE = $(TZ_BOOTLOADER)/NS/ls-ns.ld

SECURE_LDFLAGS = -T $(SECURE_LINKER_FILE)
NONSECURE_LDFLAGS = -T $(NONSECURE_LINKER_FILE)

build/bootloader/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@