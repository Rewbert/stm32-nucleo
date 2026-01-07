include mk/common/toolchain.mk

# directories in which sources are found
TZ_BOOTLOADER := TZ-bootloader
TZ_APP := TZ-app

# building the secure elf will produce this lib, telling the nonsecure what NSC functions there are
SECURE_LIB = build/secure_cmse_import.lib

TZ_CFLAGS = \
  $(CFLAGS) \
  $(TRUSTZONE_AWARENESS) \
  $(NO_STDLIB) \
  $(NOSTARTFILES) \
  $(DEBUG)

SECURE_ELF := secure.elf
NONSECURE_ELF := nonsecure.elf