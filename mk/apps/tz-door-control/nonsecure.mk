include mk/TZ/tz-common.mk
include mk/TZ-lib/tz-lib.mk # here resides the TZ aware little lib I wrote

##### variables #####

# where to find the lib, for use in include
TZ_LIB := TZ-lib

# where the application sources are found
TZ_APP := apps/tz-door-control

# name of the final .elf's
NONSECURE_ELF := nonsecure-dc.elf

# these are the source files
NONSECURE_SRC := \
  $(TZ_BOOTLOADER)/NS/src/bootloader_ns.c \
  $(TZ_APP)/NS/main.c \
  $(TZ_APP)/shared/shared.c

# and here are the corresponding object files
NONSECURE_O := $(patsubst %.c, build/nonsecure/%.o, $(NONSECURE_SRC))

# the includes required for compilation
NONSECURE_INC := -I$(TZ_LIB)/include -I$(TZ_APP)/shared

NONSECURE_CPPFLAGS = \
  $(CPPFLAGS) \
  $(NONSECURE_INC)

NONSECURE_LINKER_FILE = $(TZ_BOOTLOADER)/NS/ls-ns.ld
NONSECURE_LDFLAGS = -T $(NONSECURE_LINKER_FILE)

# rules

build/nonsecure/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(NONSECURE_CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@

# final target

$(NONSECURE_ELF): $(NONSECURE_O) $(NONSECURE_A)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ $^ $(SECURE_LIB)