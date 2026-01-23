include mk/TZ/tz-common.mk
include mk/TZ-lib/tz-lib.mk # here resides the TZ aware little lib I wrote

##### variables #####

# where to find the lib, for use in include
TZ_LIB := TZ-lib

# where the application sources are found
TZ_APP := apps/tz-door-control

# name of the final .elf's
SECURE_ELF := secure-dc.elf

# these are the source files
SECURE_SRC := \
  $(TZ_BOOTLOADER)/S/src/bootloader.c \
  $(TZ_BOOTLOADER)/S/src/security_config.c \
  $(TZ_APP)/S/main.c \
  $(TZ_APP)/shared/shared.c

# and here are the corresponding object files
SECURE_O := $(patsubst %.c, build/secure/%.o, $(SECURE_SRC))

# the includes required for compilation
SECURE_INC := -I$(TZ_LIB)/include -I$(TZ_APP)/shared

# I am not sure why there are commas here, but writing it likes this passes this whole thing in as a comma separated string (which is correct)s
IMPLIB_FLAGS = -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

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
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(IMPLIB_FLAGS)