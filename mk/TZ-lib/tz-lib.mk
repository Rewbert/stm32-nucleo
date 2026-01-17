include mk/common/toolchain.mk

# variables

TZ_LIB := TZ-lib
LIB_SRC := \
  $(TZ_LIB)/src/hal/config/gpio.c \
  $(TZ_LIB)/src/hal/config/clock.c \
  $(TZ_LIB)/src/hal/config/exti.c \
  $(TZ_LIB)/src/hal/config/uart.c \
  $(TZ_LIB)/src/hal/drivers/gpio.c \
  $(TZ_LIB)/src/hal/platform/clock.c \
  $(TZ_LIB)/src/hal/platform/uart.c \
  $(TZ_LIB)/src/hal/platform/flash.c

LIB_INC := -I$(TZ_LIB)/include

LIB_SECURE_O    := $(patsubst %.c,build/s/%.o,  $(LIB_SRC))
LIB_NONSECURE_O := $(patsubst %.c,build/ns/%.o, $(LIB_SRC))

LIB_CFLAGS := $(CFLAGS) $(TRUSTZONE_AWARENESS) $(NO_STDLIB) $(NOSTARTFILES)
LIB_CPPFLAGS := $(CPPFLAGS) $(LIB_INC)

SECURE_LIB := build/tz-lib-secure.a
NONSECURE_LIB := build/tz-lib-nonsecure.a

# rules

build/s/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(LIB_CFLAGS) $(LIB_CPPFLAGS) -DSECURE -c $< -o $@

build/ns/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(LIB_CFLAGS) $(LIB_CPPFLAGS) -c $< -o $@

# final build targets

$(SECURE_LIB): $(LIB_SECURE_O)
	$(AR) rcs $@ $(LIB_SECURE_O)

$(NONSECURE_LIB): $(LIB_NONSECURE_O)
	$(AR) rcs $@ $(LIB_NONSECURE_O)

.PHONY: all

all: $(SECURE_LIB) $(NONSECURE_LIB)