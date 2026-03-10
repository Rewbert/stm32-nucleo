ifndef MK_DRIVERS_BOARD
MK_DRIVERS_BOARD := 1

include mk-drivers/bootloader.mk

###
# This makefile builds the board object. Your application should be linked with the drivers, the bootloader, and
# a board object. I will need to modify this makefile when I add support for my second board (STM32U5), unless
# the BOARD binding works well enough.
###

BOARD_SRC := boards/$(BOARD)/board.c
BOARD_O   := build/s/boards/$(BOARD)/board.o

build/s/boards/%.o: boards/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

BOARD_NS_O := build/ns/boards/$(BOARD)/board.o

build/ns/boards/%.o: boards/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

endif
