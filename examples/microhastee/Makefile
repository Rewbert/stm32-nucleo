include mk-drivers/board.mk
include mk-drivers/udb.mk
include mk-drivers/mhs.mk

###
# Build the MicroHasTEE example: MicroHaskell running in both the secure and
# non-secure worlds using the firmware/drivers and firmware/boards framework.
###

APP       := examples/microhastee
MHS_BIN_ABS := $(abspath $(MHS_BIN))

# Extra include paths on top of what board.mk provides.
# Secure build prepends -I$(APP)/S so the shadow config.h there is found first.
# $(NOR_UDB_INC) is for the udb key-value store (out-of-tree -- see
# mk-drivers/udb.mk), consumed via examples/microhastee/shared/udb-drv.c and
# HAL/Drivers/UDB.hs -- STM32U5 only for now, see examples/microhastee/shared/udb-drv.c.
MHS_INC_S  := -I$(MHS_DIR) -I$(APP)/S -I$(APP)/shared $(NOR_UDB_INC)
MHS_INC_NS := -I$(MHS_DIR) -I$(APP)/shared $(NOR_UDB_INC)

SECURE_CPPFLAGS    += $(MHS_INC_S)
NONSECURE_CPPFLAGS += $(MHS_INC_NS)

SECURE_LIB          := build/$(APP)/secure_cmse_import.lib
SECURE_IMPLIB_FLAGS := -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

# --- Haskell compilation rules ---

HASKELL_DIR := $(APP)/haskell
HASKELL_SRC = \
  $(HASKELL_DIR)/src/Control/Monad/IO/Class.hs \
  $(HASKELL_DIR)/src/Control/Monad/IxMonad.hs \
  $(HASKELL_DIR)/src/Control/Monad/State/Class.hs \
  $(HASKELL_DIR)/src/Control/Monad/State.hs \
  $(HASKELL_DIR)/src/Foreign/HAL/Utils.hs \
  $(HASKELL_DIR)/src/Effectful/Internal/NonSecure.hs \
  $(HASKELL_DIR)/src/Effectful/Internal/Secure.hs \
  $(HASKELL_DIR)/src/Effectful/Internal/Setup.hs \
  $(HASKELL_DIR)/src/Effectful/TypeLevel/List.hs \
  $(HASKELL_DIR)/src/Effectful/TypeLevel/Lock.hs \
  $(HASKELL_DIR)/src/Effectful/TypeLevel/Number.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/GPIO.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/UART.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/TZSC.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/SysTick.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/Board.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/RCC.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/NVIC.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/EXTI.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/IRQ.hs \
  $(HASKELL_DIR)/src/Effectful/HAL/UDB.hs \
  $(HASKELL_DIR)/src/Effectful/HAL.hs \
  $(HASKELL_DIR)/src/Effectful/NonSecure.hs \
  $(HASKELL_DIR)/src/Effectful/Secure.hs \
  $(HASKELL_DIR)/src/Effectful/Setup.hs \
  $(HASKELL_DIR)/src/MicroHasTEE.hs \
  $(HASKELL_DIR)/src/HAL.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/SysTick.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/GPIO.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/UART.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/RCC.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/PWR.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/FLASH.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/EXTI.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/NVIC.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/TZSC.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/IRQ.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/UDB/Internal.hs \
  $(HASKELL_DIR)/src/HAL/Drivers/UDB.hs \
  $(HASKELL_DIR)/src/HAL/Board/Board.hs \
  $(HASKELL_DIR)/src/HAL/Board/LED.hs \
  $(HASKELL_DIR)/src/HAL/Board/Button.hs \
  $(HASKELL_DIR)/app/DoorLock.hs \
  $(HASKELL_DIR)/app/MatrixKeypad.hs \
  $(HASKELL_DIR)/app/SimpleExample.hs

CABAL=mcabal

# Which Haskell module under app/ to build as the application. Overridden by
# the `simple` target below. gen2.c is gitignored and only regenerated when
# $(HASKELL_SRC) changes, and that list doesn't encode MAIN_HS -- so switching
# MAIN_HS alone (e.g. all <-> simple) wouldn't be noticed on its own.
# check-main-hs (a prerequisite of all/simple, below) records which module was
# last built and deletes gen2.c to force a regen whenever that changes.
MAIN_HS ?= MatrixKeypad
MAIN_HS_RECORD := build/$(APP)/.main-hs

.PHONY: check-main-hs
check-main-hs:
	mkdir -p $(dir $(MAIN_HS_RECORD))
	if [ "$$(cat $(MAIN_HS_RECORD) 2>/dev/null)" != "$(MAIN_HS)" ]; then \
		rm -f $(APP)/S/gen2.c $(APP)/NS/gen2.c; \
		echo $(MAIN_HS) > $(MAIN_HS_RECORD); \
	fi

# mhs's compilation cache (.mhscache) is a single hardcoded filename, read/written
# from whatever directory mhs is invoked in -- it has no notion of build variants
#
# One cache per variant, kept in build/, swapped into $(HASKELL_DIR)/.mhscache
# around each compile.
MHS_CACHE_S  := build/$(APP)/mhs-cache-s/.mhscache
MHS_CACHE_NS := build/$(APP)/mhs-cache-ns/.mhscache

$(APP)/S/gen2.c: $(HASKELL_SRC)
	mkdir -p $(dir $(MHS_CACHE_S))
	if [ -f $(MHS_CACHE_S) ]; then cp -f $(MHS_CACHE_S) $(HASKELL_DIR)/.mhscache; else rm -f $(HASKELL_DIR)/.mhscache; fi
	cd $(HASKELL_DIR) && $(MHS_BIN_ABS) -DSECURE -C -isrc -iapp -c -ogen.c app/$(MAIN_HS).hs && cp gen.c ../S/gen2.c && rm gen.c
	cp -f $(HASKELL_DIR)/.mhscache $(MHS_CACHE_S)

# Order-only on S/gen2.c: both variants swap the same $(HASKELL_DIR)/.mhscache
# into place, so they must not build concurrently under `make -j`.
$(APP)/NS/gen2.c: $(HASKELL_SRC) | $(APP)/S/gen2.c
	mkdir -p $(dir $(MHS_CACHE_NS))
	if [ -f $(MHS_CACHE_NS) ]; then cp -f $(MHS_CACHE_NS) $(HASKELL_DIR)/.mhscache; else rm -f $(HASKELL_DIR)/.mhscache; fi
	cd $(HASKELL_DIR) && $(MHS_BIN_ABS) -C -isrc -iapp -ogen.c app/$(MAIN_HS).hs && cp gen.c ../NS/gen2.c && rm gen.c
	cp -f $(HASKELL_DIR)/.mhscache $(MHS_CACHE_NS)

# --- Secure application ---

SECURE_SRC :=            \
  $(APP)/S/main.c        \
  $(APP)/S/gen2.c        \
  $(APP)/S/vtable.c      \
  $(APP)/shared/config.c \
  $(APP)/shared/stubs.c  \
  $(APP)/shared/extra.c  \
  $(APP)/shared/exti_trampoline.c \
  $(APP)/shared/flash-drv.c \
  $(APP)/shared/udb-drv.c \
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
  $(APP)/shared/exti_trampoline.c \
  $(APP)/shared/flash-drv.c \
  $(APP)/shared/udb-drv.c \
  $(MHS_DIR)/eval.c

NONSECURE_O := $(patsubst %.c, build/ns/%.o, $(NONSECURE_SRC))

build/ns/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

build/ns/$(MHS_DIR)/%.o: $(MHS_DIR)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

# --- Link targets ---

.PHONY: all simple
all: check-main-hs secure.elf nonsecure.elf

# Same as `all`, but builds app/SimpleExample.hs instead of app/DoorLock.hs.
simple: MAIN_HS := SimpleExample
simple: check-main-hs secure.elf nonsecure.elf

$(dir $(SECURE_LIB)):
	mkdir -p $@

secure.elf: $(SECURE_BOOT_O) $(BOARD_O) $(SECURE_O) $(TZ_LIB_DRV_SECURE_A) $(NOR_UDB_A) | $(dir $(SECURE_LIB))
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(SECURE_IMPLIB_FLAGS)

nonsecure.elf: secure.elf $(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NONSECURE_O) $(TZ_LIB_DRV_NONSECURE_A) $(NOR_UDB_A)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ \
		$(NONSECURE_BOOT_O) $(BOARD_NS_O) $(NONSECURE_O) $(TZ_LIB_DRV_NONSECURE_A) $(NOR_UDB_A) $(SECURE_LIB)
