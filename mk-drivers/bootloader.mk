ifndef MK_DRIVERS_BOOTLOADER
MK_DRIVERS_BOOTLOADER := 1

include mk-drivers/drivers.mk

###
# This makefile builds the bootloader. It depends on the drivers as it needs to access the MPCBB driver.
# For now, there is only a bootloader for the secure application. I will add support for nonsecure in
# a couple of days.
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

SECURE_LDFLAGS := -T $(TZ_DRV_BOOT)/S/ls-s.ld

SECURE_BOOT_SRC := \
  $(TZ_DRV_BOOT)/S/src/bootloader.c \
  $(TZ_DRV_BOOT)/S/src/stm32l5/tz_init.c

SECURE_BOOT_O := $(patsubst %.c, build/s/%.o, $(SECURE_BOOT_SRC))

build/s/$(TZ_DRV_BOOT)/%.o: $(TZ_DRV_BOOT)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

endif
