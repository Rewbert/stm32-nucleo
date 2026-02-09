include mk/apps/tz-mhs-both/secure.mk

##### variables #####

# where to find the lib, for use in include
TZ_LIB := TZ-lib

# where the application sources are found
TZ_MHS_APP := apps/tz-mhs-both

# name of the final .elf's
NONSECURE_MHS_ELF := nonsecure-mhs.elf

MHS := MicroHs
MHS_DIR := $(MHS)/src/runtime

NONSECURE_MHS_SRC := \
  $(TZ_MHS_APP)/NS/config.c \
  $(TZ_MHS_APP)/NS/extra.c \
  $(TZ_MHS_APP)/NS/main.c \
  $(TZ_MHS_APP)/NS/gen2.c \
  $(TZ_MHS_APP)/NS/stubs.c \
  $(MHS_DIR)/eval.c

NONSECURE_MHS_O := $(patsubst %.c, build/tz-mhs-both/NS/%.o, $(NONSECURE_MHS_SRC))

# the includes required for compilation
NONSECURE_MHS_INC := -I$(TZ_LIB)/include -I$(MHS_DIR) -I$(TZ_MHS_APP)/NS

NONSECURE_MHS_CPPFLAGS = \
  $(CPPFLAGS) \
  $(NONSECURE_MHS_INC) \

TZ_CFLAGS = \
  $(CFLAGS) \
  $(TRUSTZONE_AWARENESS) \
  $(NO_STDLIB) \
  $(NOSTARTFILES) \
  $(DEBUG)

# rules

build/tz-mhs-both/NS/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(NONSECURE_MHS_CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@

# final target

$(NONSECURE_MHS_ELF): $(SECURE_MHS_ELF) $(NONSECURE_BOOTLOADER_O) $(NONSECURE_MHS_O) $(NONSECURE_A)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_MHS_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ $(NONSECURE_BOOTLOADER_O) $(NONSECURE_MHS_O) $(NONSECURE_A) $(SECURE_MHS_LIB)