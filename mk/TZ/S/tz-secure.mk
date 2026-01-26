include mk/TZ/tz-bootloader.mk

# variables

SECURE_SRC := \
  $(TZ_APP)/S/src/app.c \
  $(TZ_APP)/S/src/persist.c

SECURE_O := $(patsubst %.c, build/secure/%.o, $(SECURE_SRC))

SECURE_INC := -I$(TZ_APP)/S/inc

# I am not sure why there are commas here, but writing it likes this passes this whole thing in as a comma separated string (which is correct)s
IMPLIB_FLAGS = -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

SECURE_CPPFLAGS = \
  $(CPPFLAGS) \
  $(SECURE_INC) \
  -DSECURE

SECURE_ELF := secure.elf

# building the secure elf will produce this lib, telling the nonsecure what NSC functions there are
SECURE_LIB = build/secure_cmse_import.lib

# rules

build/secure/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(SECURE_CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@

# final target

# TODO this builds two things, the elf and the lib. Ask someone if I can indicate that these two are built by this one rule
$(SECURE_ELF): $(SECURE_BOOTLOADER_O) $(SECURE_O)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(IMPLIB_FLAGS)