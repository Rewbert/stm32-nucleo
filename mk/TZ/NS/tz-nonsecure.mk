include mk/TZ/tz-common.mk
include mk/TZ/S/tz-secure.mk

# variables

NONSECURE_SRC := \
  $(TZ_BOOTLOADER)/NS/src/bootloader_ns.c \
  $(TZ_APP)/NS/src/app_ns.c \
  $(TZ_APP)/NS/src/test.c

NONSECURE_O := $(patsubst %.c, build/nonsecure/%.o, $(NONSECURE_SRC))

NONSECURE_INC := -I$(TZ_APP)/NS/inc

NONSECURE_CPPFLAGS += \
  $(CPPFLAGS) \
  $(NONSECURE_INC)

NONSECURE_LINKER_FILE = $(TZ_BOOTLOADER)/NS/ls-ns.ld
NONSECURE_LDFLAGS = -T $(NONSECURE_LINKER_FILE)

NONSECURE_ELF := nonsecure.elf

# rules

build/nonsecure/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(NONSECURE_CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@

# final target

$(NONSECURE_ELF): $(SECURE_ELF) $(NONSECURE_O)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ $(NONSECURE_O) $(SECURE_LIB)