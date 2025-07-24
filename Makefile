##### define the target to build #####

# there are two targets, main and mhs

# We can call make clean or make help, or make openocd without specifying the TARGET
ifneq ($(filter-out clean help openocd secure.elf nonsecure.elf read_option_bytes,$(MAKECMDGOALS)),)
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

##### my little mini OS source files, always included for now #####

CMSIS_SRC = CMSIS/Device/ST/STM32L5/Source/Templates/system_stm32l5xx

ROBOS = robos
ROBOS_SRC = $(ROBOS)/src/syscalls $(ROBOS)/src/startup $(ROBOS)/src/clock $(ROBOS)/src/uart $(ROBOS)/src/gpio $(ROBOS)/src/timer
ROBOS_INC = $(ROBOS)/include

#####

CC=arm-none-eabi-gcc
CFLAGS=-mcpu=cortex-m33 -mthumb --specs=nano.specs -g # can remove -g later
CPPFLAGS=-DSTM32L552xx \
     -ICMSIS/Device/ST/STM32L5/Include \
	 -ICMSIS/CMSIS/Core/Include \
	 $(if $(ROBOS_INC), -I$(ROBOS_INC)) \
	 $(if $(TARGET_INC), -I$(TARGET_INC))

# place built object files in here, .o files are placed directly in here. Perhaps it would be better to create subdirs, like .build/.object/?
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

# for this, you need STM32_Programmer_CLI from ST. Download it here: https://www.st.com/en/development-tools/stm32cubeprog.html
# then unpack, and make a symlink so that the tool is reachable everywhere
# I did sudo ln -s <full-path-to-STM32_Programmer_CLI> /usr/bin/STM32_Programmer_CLI
read_option_bytes:
	STM32_Programmer_CLI -c port=SWD -ob displ

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
	rm -r $(BUILD_DIR) *.elf $(SECURE_LIB)














###### TrustZone experiments #####

### BUILDING THE SECURE ELF ###

# path to the secure project
SECURE = TZ/S
SECURE_APP_SRC = $(SECURE)/src/startup.c # add the secure sources here, one by one
SECURE_SRC = $(SECURE_APP_SRC) $(CMSIS_SRC).c

SECURE_LIB = secure_cmse_import.lib

SECURE_BUILD_DIR = .build/S

# this creates the secure object names. .build/startup.o etc
SECURE_OBJS = $(addprefix $(SECURE_BUILD_DIR)/, $(addsuffix .o, $(notdir $(basename $(SECURE_SRC)))))

# -mcmse is the thing that makes the compiler aware of trustzone
# no stdlib for now
SECURE_CC_FLAGS = -mcpu=cortex-m33 -mthumb -mcmse --specs=nosys.specs -nostartfiles

SECURE_LINKER_FILE = $(SECURE)/ls-s.ld
SECURE_LDFLAGS = -T $(SECURE_LINKER_FILE)

SECURE_CPPFLAGS=-DSTM32L552xx \
                -ICMSIS/Device/ST/STM32L5/Include \
	              -ICMSIS/CMSIS/Core/Include

# I am not sure why there are commas here, but writing it likes this passes this whole thing in as a comma separated string (which is correct)s
SECURE_IMPLIB_FLAGS = -Wl,--cmse-implib,--out-implib=$(SECURE_LIB)

# $< refers to the first prerequisites, which are the .c files. $@ refers to the target, the object files
$(SECURE_BUILD_DIR)/%.o: $(SECURE)/src/%.c | $(SECURE_BUILD_DIR)
	$(CC) $(SECURE_CC_FLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

$(SECURE_BUILD_DIR)/%.o: CMSIS/Device/ST/STM32L5/Source/Templates/%.c | $(SECURE_BUILD_DIR)
	$(CC) $(SECURE_CC_FLAGS) $(SECURE_CPPFLAGS) -c $< -o $@

$(SECURE_BUILD_DIR):
	mkdir -p $(SECURE_BUILD_DIR)

secure.elf: $(SECURE_OBJS)
	$(CC) $(SECURE_CC_FLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^ $(SECURE_IMPLIB_FLAGS)

### BUILDING THE NONSECURE ELF ###

NONSECURE = TZ/NS
NONSECURE_APP_SRC = $(NONSECURE)/src/startup.c $(NONSECURE)/src/test.c
NONSECURE_SRC = $(NONSECURE_APP_SRC) $(CMSIS_SRC).c

NONSECURE_BUILD_DIR = .build/NS

# this creates the nonsecure object names. .build/startup.o etc
NONSECURE_OBJS = $(addprefix $(NONSECURE_BUILD_DIR)/, $(addsuffix .o, $(notdir $(basename $(NONSECURE_SRC)))))

# -mcmse is the thing that makes the compiler aware of trustzone
# no stdlib for now
NONSECURE_CC_FLAGS = -mcpu=cortex-m33 -mthumb -mcmse --specs=nosys.specs -nostartfiles

NONSECURE_LINKER_FILE = $(NONSECURE)/ls-ns.ld
NONSECURE_LDFLAGS = -T $(NONSECURE_LINKER_FILE)

NONSECURE_CPPFLAGS=-DSTM32L552xx \
                -ICMSIS/Device/ST/STM32L5/Include \
	              -ICMSIS/CMSIS/Core/Include

$(NONSECURE_BUILD_DIR)/%.o: $(NONSECURE)/src/%.c | $(NONSECURE_BUILD_DIR)
	$(CC) $(NONSECURE_CC_FLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

$(NONSECURE_BUILD_DIR)/%.o: CMSIS/Device/ST/STM32L5/Source/Templates/%.c | $(NONSECURE_BUILD_DIR)
	$(CC) $(NONSECURE_CC_FLAGS) $(NONSECURE_CPPFLAGS) -c $< -o $@

$(NONSECURE_BUILD_DIR):
	mkdir -p $(NONSECURE_BUILD_DIR)

# because the secure.elf build process also builds the secure lib, we need to build it first
nonsecure.elf: secure.elf $(NONSECURE_OBJS)
	$(CC) $(NONSECURE_CC_FLAGS) $(NONSECURE_CPPFLAGS) $(NONSECURE_LDFLAGS) -o $@ $(NONSECURE_OBJS) $(SECURE_LIB)