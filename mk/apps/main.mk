include mk/TZ/tz-common.mk
include mk/TZ-lib/tz-lib.mk # here resides the TZ aware little lib I wrote

# This makefile builds a simple example for the STM32L552ZEQ Nucleo board, showing how to use the
# basic functionalities of the little library I wrote to access the functionalities of the board.
#
# While trustzone 'executes' both a secure and nonsecure elf, this little example only shows the library, and
# does not build a nonsecure application.

##### variables #####

# where to find the lib, for use in include
TZ_LIB := TZ-lib

# where the application sources are found
TZ_APP := apps/main

# name of the final .elf
SECURE_ELF := secure-main.elf

# these are the source files
SECURE_SRC := \
  $(TZ_BOOTLOADER)/S/src/bootloader.c \
  $(TZ_BOOTLOADER)/S/src/security_config.c \
  $(TZ_APP)/S/main.c \

# and here are the corresponding object files
SECURE_O := $(patsubst %.c, build/secure/%.o, $(SECURE_SRC))

# the includes required for compilation (only the library this time, since the app is so simple)
SECURE_INC := -I$(TZ_LIB)/include

SECURE_CPPFLAGS = \
  $(CPPFLAGS) \
  $(SECURE_INC) \
  -DSECURE # need the SECURE flag, to make sure the library only includes code intended to execute in secure world

SECURE_LINKER_FILE = $(TZ_BOOTLOADER)/S/ls-s.ld
SECURE_LDFLAGS = -T $(SECURE_LINKER_FILE)

# rules

build/secure/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(SECURE_CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@

# final target

# TODO this builds two things, the elf and the lib. Ask someone if I can indicate that these two are built by this one rule
$(SECURE_ELF): $(SECURE_O) $(SECURE_A)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^