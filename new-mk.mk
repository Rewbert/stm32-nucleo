ifndef TZ_LIB_DRV_MK
TZ_LIB_DRV_MK := 1

include mk/common/toolchain.mk

BOARD ?= stm32l5

TZ_LIB_DRV     := TZ-lib-drivers
TZ_LIB_DRV_SRC := $(wildcard $(TZ_LIB_DRV)/src/drivers/$(BOARD)/*.c) $(wildcard $(TZ_LIB_DRV)/src/drivers/*.c)

TZ_LIB_DRV_INC      := -I$(TZ_LIB_DRV)/include
TZ_LIB_DRV_CFLAGS   := $(CFLAGS) $(TRUSTZONE_AWARENESS) $(NO_STDLIB) $(NOSTARTFILES) $(DEBUG)
TZ_LIB_DRV_CPPFLAGS := $(CPPFLAGS) $(TZ_LIB_DRV_INC)

TZ_LIB_DRV_SECURE_A    := build/tz-lib-drivers-secure.a
TZ_LIB_DRV_NONSECURE_A := build/tz-lib-drivers-nonsecure.a

TZ_LIB_DRV_SECURE_O    := $(patsubst $(TZ_LIB_DRV)/%.c,build/s/$(TZ_LIB_DRV)/%.o,$(TZ_LIB_DRV_SRC))
TZ_LIB_DRV_NONSECURE_O := $(patsubst $(TZ_LIB_DRV)/%.c,build/ns/$(TZ_LIB_DRV)/%.o,$(TZ_LIB_DRV_SRC))

.PHONY: all
all: $(TZ_LIB_DRV_SECURE_A) $(TZ_LIB_DRV_NONSECURE_A)

build/s/$(TZ_LIB_DRV)/%.o: $(TZ_LIB_DRV)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_LIB_DRV_CFLAGS) $(TZ_LIB_DRV_CPPFLAGS) -DSECURE -c $< -o $@

build/ns/$(TZ_LIB_DRV)/%.o: $(TZ_LIB_DRV)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_LIB_DRV_CFLAGS) $(TZ_LIB_DRV_CPPFLAGS) -c $< -o $@

$(TZ_LIB_DRV_SECURE_A): $(TZ_LIB_DRV_SECURE_O)
	$(AR) rcs $@ $^

$(TZ_LIB_DRV_NONSECURE_A): $(TZ_LIB_DRV_NONSECURE_O)
	$(AR) rcs $@ $^

endif
