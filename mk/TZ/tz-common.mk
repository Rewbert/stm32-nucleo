include mk/common/toolchain.mk

# directories in which sources are found
TZ_BOOTLOADER := TZ-bootloader
TZ_APP := TZ-app

TZ_CFLAGS = \
  $(CFLAGS) \
  $(TRUSTZONE_AWARENESS) \
  $(NO_STDLIB) \
  $(NOSTARTFILES) \
  $(DEBUG)