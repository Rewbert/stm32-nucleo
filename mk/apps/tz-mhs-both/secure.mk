include mk/TZ/tz-bootloader.mk
include mk/TZ-lib/tz-lib.mk

##### variables #####

# where to find the lib, for use in include
TZ_LIB := TZ-lib

# where the application sources are found
TZ_MHS_APP := apps/tz-mhs-both

# name of the final .elf's
SECURE_MHS_ELF := secure-mhs.elf

MHS := MicroHs
MHS_DIR := $(MHS)/src/runtime

SECURE_MHS_SRC := \
  $(TZ_MHS_APP)/S/config.c \
  $(TZ_MHS_APP)/S/extra.c \
  $(TZ_MHS_APP)/S/main.c \
  $(TZ_MHS_APP)/S/gen2.c \
  $(TZ_MHS_APP)/S/stubs.c \
  $(MHS_DIR)/eval.c

SECURE_MHS_O := $(patsubst %.c, build/tz-mhs-both/S/%.o, $(SECURE_MHS_SRC))

# the includes required for compilation
SECURE_MHS_INC := -I$(TZ_LIB)/include -I$(MHS_DIR) -I$(TZ_MHS_APP)/S

SECURE_MHS_LIB = build/apps/tz-mhs-both/secure_cmse_import.lib

# I am not sure why there are commas here, but writing it likes this passes this whole thing in as a comma separated string (which is correct)s
MHS_IMPLIB_FLAGS = -Wl,--cmse-implib,--out-implib=$(SECURE_MHS_LIB)

SECURE_MHS_CPPFLAGS = \
  $(CPPFLAGS) \
  $(SECURE_MHS_INC) \
  -DSECURE # need the SECURE flag, to make sure the library only includes code intended to execute in secure world

TZ_CFLAGS = \
  $(CFLAGS) \
  $(TRUSTZONE_AWARENESS) \
  $(NO_STDLIB) \
  $(NOSTARTFILES) \
  $(DEBUG)

# rules

$(dir $(SECURE_MHS_LIB)):
	mkdir -p $@

build/tz-mhs-both/S/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(SECURE_MHS_CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@

# final target

# TODO this builds two things, the elf and the lib. Ask someone if I can indicate that these two are built by this one rule
$(SECURE_MHS_ELF): $(SECURE_BOOTLOADER_O) $(SECURE_MHS_O) $(SECURE_A) | $(dir $(SECURE_MHS_LIB))
	$(CC) $(TZ_CFLAGS) $(SECURE_MHS_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(MHS_IMPLIB_FLAGS)