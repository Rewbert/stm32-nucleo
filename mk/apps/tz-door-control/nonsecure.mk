include mk/apps/tz-door-control/secure.mk

##### variables #####

# where the application sources are found
TZ_APP := apps/tz-door-control

# name of the final .elf's
TDZA_NS_NONSECURE_ELF := nonsecure-dc.elf

# these are the source files
TDZA_NS_NONSECURE_SRC := \
  $(TZ_BOOTLOADER)/NS/src/bootloader_ns.c \
  $(TZ_APP)/NS/main.c \
  $(TZ_APP)/shared/shared.c

# and here are the corresponding object files
TDZA_NS_NONSECURE_O := $(patsubst %.c, build/%.o, $(TDZA_NS_NONSECURE_SRC))

# the includes required for compilation
TDZA_NS_NONSECURE_INC := -I$(TZ_LIB)/include -I$(TZ_APP)/shared

TDZA_NS_NONSECURE_CPPFLAGS = \
  $(CPPFLAGS) \
  $(TDZA_NS_NONSECURE_INC)

TDZA_NS_NONSECURE_LINKER_FILE = $(TZ_BOOTLOADER)/NS/ls-ns.ld
TDZA_NS_NONSECURE_LDFLAGS = -T $(TDZA_NS_NONSECURE_LINKER_FILE)

# rules

build/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(TDZA_NS_NONSECURE_CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@

# final target

$(TDZA_NS_NONSECURE_ELF): $(TZDA_SECURE_ELF) $(TDZA_NS_NONSECURE_O) $(NONSECURE_A)
	$(CC) $(TZ_CFLAGS) $(TDZA_NS_NONSECURE_CPPFLAGS) $(TDZA_NS_NONSECURE_LDFLAGS) -o $@ $(TDZA_NS_NONSECURE_O) $(NONSECURE_A) $(TZDA_SECURE_LIB)