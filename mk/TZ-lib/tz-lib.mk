ifndef TZ_LIB_MK
TZ_LIB_MK := 1

include mk/common/toolchain.mk

# variables

TZ_LIB := TZ-lib
LIB_SRC := \
  $(TZ_LIB)/src/hal/config/gpio.c \
  $(TZ_LIB)/src/hal/config/clock.c \
  $(TZ_LIB)/src/hal/config/exti.c \
  $(TZ_LIB)/src/hal/config/uart.c \
  $(TZ_LIB)/src/hal/drivers/gpio.c \
  $(TZ_LIB)/src/hal/drivers/uart.c \
  $(TZ_LIB)/src/hal/platform/clock.c \
  $(TZ_LIB)/src/hal/platform/uart.c \
  $(TZ_LIB)/src/hal/platform/flash.c \
  $(TZ_LIB)/src/hal/platform/exti.c \
  $(TZ_LIB)/src/services/button.c \
  $(TZ_LIB)/src/services/led.c \
  $(TZ_LIB)/src/services/uart.c

LIB_INC := -I$(TZ_LIB)/include

LIB_SECURE_O    := $(patsubst %.c,build/s/%.o,  $(LIB_SRC))
LIB_NONSECURE_O := $(patsubst %.c,build/ns/%.o, $(LIB_SRC))

LIB_CFLAGS := $(CFLAGS) $(TRUSTZONE_AWARENESS) $(NO_STDLIB) $(NOSTARTFILES)
LIB_CPPFLAGS := $(CPPFLAGS) $(LIB_INC)

SECURE_A := build/tz-lib-secure.a
NONSECURE_A := build/tz-lib-nonsecure.a

# rules

build/s/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(LIB_CFLAGS) $(LIB_CPPFLAGS) -DSECURE -c $< -o $@

build/ns/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(LIB_CFLAGS) $(LIB_CPPFLAGS) -c $< -o $@

# final build targets

$(SECURE_A): $(LIB_SECURE_O)
	$(AR) rcs $@ $(LIB_SECURE_O)

$(NONSECURE_A): $(LIB_NONSECURE_O)
	$(AR) rcs $@ $(LIB_NONSECURE_O)

.PHONY: all

all: $(SECURE_A) $(NONSECURE_A)

endif