# STM32U5 only, per this project's board primacy.
BOARD := stm32u5

include mk-drivers/board.mk

###
# Build the solenoid-test example: a secure-only board bring-up scaffold
# (board init, PLL, SysTick, UART, IRQs) with no application logic yet.
###

APP := examples/solenoid-test

SRC := $(APP)/main.c
OBJ := $(patsubst %.c, build/s/%.o, $(SRC))

build/s/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

.PHONY: all
all: secure.elf

secure.elf: $(SECURE_BOOT_O) $(BOARD_O) $(OBJ) $(TZ_LIB_DRV_SECURE_A)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^
