include mk/TZ/tz-common.mk
include mk/TZ-lib/tz-lib.mk # here resides the TZ aware little lib I wrote

##### variables #####

# where to find the lib, for use in include
TZ_LIB := TZ-lib

# where the application sources are found
TZ_APP := apps/tz-door-control

# name of the final .elf's
TZDA_SECURE_ELF := secure-dc.elf

# these are the source files
TZDA_SECURE_SRC := \
  $(TZ_BOOTLOADER)/S/src/bootloader.c \
  $(TZ_BOOTLOADER)/S/src/security_config.c \
  $(TZ_APP)/S/main.c \
  $(TZ_APP)/shared/shared.c

# and here are the corresponding object files
TZDA_SECURE_O := $(patsubst %.c, build/%.o, $(TZDA_SECURE_SRC))

# the includes required for compilation
TZDA_SECURE_INC := -I$(TZ_LIB)/include -I$(TZ_APP)/shared

# building the secure elf will produce this lib, telling the nonsecure what NSC functions there are
TZDA_SECURE_LIB = build/apps/tz-door-control/secure_cmse_import.lib

# I am not sure why there are commas here, but writing it likes this passes this whole thing in as a comma separated string (which is correct)s
TZDA_IMPLIB_FLAGS = -Wl,--cmse-implib,--out-implib=$(TZDA_SECURE_LIB)

TZDA_SECURE_CPPFLAGS = \
  $(CPPFLAGS) \
  $(TZDA_SECURE_INC) \
  -DSECURE # need the SECURE flag, to make sure the library only includes code intended to execute in secure world

TZDA_SECURE_LINKER_FILE = $(TZ_BOOTLOADER)/S/ls-s.ld
TZDA_SECURE_LDFLAGS = -T $(TZDA_SECURE_LINKER_FILE)

# rules

$(dir $(TZDA_SECURE_LIB)):
	mkdir -p $@

build/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(TZDA_SECURE_CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@

# final target

# TODO this builds two things, the elf and the lib. Ask someone if I can indicate that these two are built by this one rule
$(TZDA_SECURE_ELF): $(TZDA_SECURE_O) $(SECURE_A) | $(dir $(TZDA_SECURE_LIB))
	$(CC) $(TZ_CFLAGS) $(TZDA_SECURE_CPPFLAGS) $(TZDA_SECURE_LDFLAGS) -o $@ $^ $(TZDA_IMPLIB_FLAGS)