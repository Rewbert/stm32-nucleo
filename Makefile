.PHONY: all

all: secure.elf nonsecure.elf main.elf mhs.elf

include mk/TZ/tz.mk
include mk/app/main.mk
include mk/app/mhs.mk

clean:
	rm -r build
	rm *.elf

#### flashing #####

PROGRAMMER = openocd
PROGRAMMER_FLAGS = -f interface/stlink.cfg -f target/stm32l5x.cfg

flash: $(FILE)
	test -n "$(FILE)" || (echo "Usage: make flash FILE=main.elf" && exit 1)
	$(PROGRAMMER) $(PROGRAMMER_FLAGS) -c "program $(FILE) verify reset exit"

##### debugging

DEBUGGER = arm-none-eabi-gdb
DEBUG_HOST = localhost
DEBUG_PORT = 3333
DEBUGGERFLAGS = -ex "target extended-remote $(DEBUG_HOST):$(DEBUG_PORT)"

$(PROGRAMMER):
	$(PROGRAMMER) $(PROGRAMMER_FLAGS) -c "gdb_port $(DEBUG_PORT)"

debug: $(FILE)
	test -n "$(FILE)" || (echo "Usage: make debug FILE=main.elf" && exit 1)
	$(DEBUGGER) $(DEBUGGERFLAGS) $(FILE)