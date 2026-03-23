include mk-drivers/board.mk

###
# Build the TZ-drivers-HasTEE application: MicroHaskell running in both the
# secure and non-secure worlds using the TZ-lib-drivers/boards framework.
###

APP       := TZ-drivers-HasTEE
MHS       := MicroHs
MHS_DIR   := $(MHS)/src/runtime
MHS_BIN   := $(MHS)/bin/mhs

# Extra include paths on top of what board.mk provides.
MHS_INC := -I$(MHS_DIR) -I$(APP)/shared

SECURE_CPPFLAGS   += $(MHS_INC)
NONSECURE_CPPFLAGS += $(MHS_INC)

SECURE_LIB          := build/$(APP)/secure_cmse_import.lib
SECURE_IMPLIB_FLAGS := -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

# --- Haskell compilation rules ---

HASKELL_DIR=HaskellApp
HASKELL_SRC = \
  $(HASKELL_DIR)/app/NonSecure.hs \
  $(HASKELL_DIR)/app/Secure.hs \
  $(HASKELL_DIR)/app/Setup.hs \
  $(HASKELL_DIR)/app/SimpleExample.hs

CABAL=mcabal

$(APP)/S/gen2.c: $(HASKELL_SRC)
	cd HaskellApp && mcabal -fsecure build && cp dist-mcabal/bin/mhs/HaskellApp.c ../$(APP)/S/gen2.c

$(APP)/NS/gen2.c: $(HASKELL_SRC)
	cd HaskellApp && mcabal build && cp dist-mcabal/bin/mhs/HaskellApp.c ../$(APP)/NS/gen2.c

# --- Secure application ---

SECURE_SRC := \
  $(APP)/S/main.c     \
  $(APP)/S/gen2.c     \
  $(APP)/shared/config.c \
  $(APP)/shared/stubs.c  \
  $(APP)/shared/extra.c  \
  $(MHS_DIR)/eval.c

SECURE_O := $(patsubst %.c, build/s/%.o, $(SECURE_SRC))

build/s/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

build/s/$(MHS_DIR)/%.o: $(MHS_DIR)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

# --- Non-secure application ---

NONSECURE_SRC := \
  $(APP)/NS/main.c    \
  $(APP)/NS/gen2.c    \
  $(APP)/shared/config.c \
  $(APP)/shared/stubs.c  \
  $(APP)/shared/extra.c  \
  $(MHS_DIR)/eval.c

NONSECURE_O := $(patsubst %.c, build/ns/%.o, $(NONSECURE_SRC))

build/ns/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

build/ns/$(MHS_DIR)/%.o: $(MHS_DIR)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

# --- Link targets ---

.PHONY: all
all: secure.elf nonsecure.elf

$(dir $(SECURE_LIB)):
	mkdir -p $@

secure.elf: $(SECURE_BOOT_O) $(BOARD_O) $(SECURE_O) $(TZ_LIB_DRV_SECURE_A) | $(dir $(SECURE_LIB))
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(SECURE_IMPLIB_FLAGS)

nonsecure.elf: secure.elf $(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NONSECURE_O) $(TZ_LIB_DRV_NONSECURE_A)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ \
		$(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NONSECURE_O) $(TZ_LIB_DRV_NONSECURE_A) $(SECURE_LIB)
