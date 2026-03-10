ifndef MK_DRIVERS_BOOTLOADER
MK_DRIVERS_BOOTLOADER := 1

include mk-drivers/drivers.mk

###
# This makefile builds the secure and nonsecure bootloaders. The secure bootloader depends on the
# drivers to access the MPCBB peripheral for TrustZone memory configuration.
###

TZ_DRV_BOOT := TZ-drivers-bootloader

TZ_CFLAGS := \
  $(CFLAGS) \
  $(TRUSTZONE_AWARENESS) \
  $(NO_STDLIB) \
  $(NOSTARTFILES) \
  $(DEBUG)

SECURE_INC := \
  $(TZ_LIB_DRV_INC) \
  -Iboards \
  -I.

SECURE_CPPFLAGS := \
  $(CPPFLAGS) \
  $(SECURE_INC) \
  -DSECURE

SECURE_LDFLAGS := -T $(TZ_DRV_BOOT)/$(BOARD)/S/ls-s.ld

SECURE_BOOT_SRC := \
  $(TZ_DRV_BOOT)/$(BOARD)/S/src/bootloader.c \
  $(TZ_DRV_BOOT)/$(BOARD)/S/src/tz_init.c

SECURE_BOOT_O := $(patsubst %.c, build/s/%.o, $(SECURE_BOOT_SRC))

build/s/$(TZ_DRV_BOOT)/%.o: $(TZ_DRV_BOOT)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

NONSECURE_CPPFLAGS := \
  $(CPPFLAGS) \
  $(SECURE_INC)

NONSECURE_LDFLAGS := -T $(TZ_DRV_BOOT)/$(BOARD)/NS/ls-ns.ld

NONSECURE_BOOT_SRC := $(TZ_DRV_BOOT)/$(BOARD)/NS/src/bootloader.c
NONSECURE_BOOT_O   := $(patsubst %.c, build/ns/%.o, $(NONSECURE_BOOT_SRC))

build/ns/$(TZ_DRV_BOOT)/%.o: $(TZ_DRV_BOOT)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

endif
