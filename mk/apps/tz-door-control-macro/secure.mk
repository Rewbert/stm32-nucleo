include mk/TZ/tz-bootloader.mk

# variables

MACRO_APP := TZ-app

MACRO_SRC := \
  $(MACRO_APP)/S/src/app.c \
  $(MACRO_APP)/S/src/persist.c

MACRO_O := $(patsubst %.c, build/tz-door-control-macro/S/%.o, $(MACRO_SRC))

MACRO_INC := -I$(MACRO_APP)/S/inc

# I am not sure why there are commas here, but writing it likes this passes this whole thing in as a comma separated string (which is correct)s
IMPLIB_FLAGS = -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

MACRO_CPPFLAGS = \
  $(CPPFLAGS) \
  $(MACRO_INC) \
  -DSECURE

SECURE_ELF := secure.elf

# building the secure elf will produce this lib, telling the nonsecure what NSC functions there are
SECURE_LIB = build/secure_cmse_import.lib

# rules

build/tz-door-control-macro/S/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(MACRO_CPPFLAGS) $(TZ_CFLAGS) -c $< -o $@

# final target

# TODO this builds two things, the elf and the lib. Ask someone if I can indicate that these two are built by this one rule
$(SECURE_ELF): $(SECURE_BOOTLOADER_O) $(MACRO_O)
	$(CC) $(TZ_CFLAGS) $(MACRO_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(IMPLIB_FLAGS)