##### define the target to build #####

# We can call make clean or make help, or make openocd without specifying the TARGET
ifneq ($(filter-out clean help openocd,$(MAKECMDGOALS)),)
  ifeq ($(origin TARGET), undefined)
    $(error TARGET is not set. Please run 'make TARGET=main <goal>' or 'make TARGET=mhs <goal>')
  endif
endif

ifneq ($(origin TARGET), undefined) # we conditionally include different sources depending on which target we are building
  ifeq ($(TARGET),main)
    TARGET_SRC = main/main
  else ifeq ($(TARGET),mhs)
    TARGET_SRC = mhs/src/eval_stm32l5 mhs/src/gen
    TARGET_INC = mhs/include
  else
    $(error Unknown TARGET: $(TARGET))
  endif
endif

##### my little mini OS source files #####

CMSIS_SRC = CMSIS/Device/ST/STM32L5/Source/Templates/system_stm32l5xx

ROBOS = robos
ROBOS_SRC = $(ROBOS)/src/syscalls $(ROBOS)/src/startup $(ROBOS)/src/clock $(ROBOS)/src/uart $(ROBOS)/src/gpio $(ROBOS)/src/timer
ROBOS_INC = $(ROBOS)/include

#####

CC=arm-none-eabi-gcc
CFLAGS=-mcpu=cortex-m33 -mthumb --specs=nano.specs -g
CPPFLAGS=-DSTM32L552xx \
     -ICMSIS/Device/ST/STM32L5/Include \
	 -ICMSIS/CMSIS/Core/Include \
	 $(if $(ROBOS_INC), -I$(ROBOS_INC)) \
	 $(if $(TARGET_INC), -I$(TARGET_INC))

# place built object files in here
BUILD_DIR=.build

# names of source files (and their location relative to root)
source_files = $(ROBOS_SRC) $(TARGET_SRC)

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