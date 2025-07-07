CC=arm-none-eabi-gcc
CFLAGS=-mcpu=cortex-m33 -mthumb --specs=nano.specs -g
CPPFLAGS=-DSTM32L552xx \
     -ICMSIS/Device/ST/STM32L5/Include \
	 -ICMSIS/CMSIS/Core/Include

# place built object files in here
BUILD_DIR=.build

# names of source files (and their location relative to root)
source_files = src/syscalls src/startup src/main CMSIS/Device/ST/STM32L5/Source/Templates/system_stm32l5xx \
               src/clock src/uart
# object files
object_files = $(addprefix $(BUILD_DIR)/, $(addsuffix .o, $(notdir $(source_files))))

LINKER_FILE=linker_script.ld
LDFLAGS=-T $(LINKER_FILE)

PROGRAMMER=openocd
PROGRAMMER_FLAGS=-f interface/stlink.cfg -f target/stm32l5x.cfg

all: prog.elf

prog.elf: $(source_files)
	$(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS) $(object_files) -o prog.elf

$(source_files): %: %.c
	mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(BUILD_DIR)/$(notdir $@).o -c $@.c

flash: prog.elf
	$(PROGRAMMER) $(PROGRAMMER_FLAGS) -c "program prog.elf verify reset exit"

# debugging targets
# to debug using GDB over openocd, run make openocd in one terminal, and make debug in another

DEBUGGER=arm-none-eabi-gdb
DEBUGGERFLAGS=-ex "target extended-remote localhost:3333"

openocd:
	openocd -f interface/stlink.cfg -f target/stm32l5x.cfg -c "gdb_port 3333"

debug: prog.elf
	$(DEBUGGER) $(DEBUGGERFLAGS) prog.elf

.PHONY: clean
clean:
	rm -r .build *.elf