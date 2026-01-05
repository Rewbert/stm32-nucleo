include mk/common/platform.mk

APP := mhs
APP_SRC := \
  $(APP)/src/eval_stm32l5.c \
  $(APP)/src/gen.c

APP_O := $(patsubst $(APP)/src/%.c,build/app/$(APP)/%.o,$(APP_SRC))

APP_INC := $(APP)/include

CPPFLAGS += -I$(APP_INC)

build/app/$(APP)/%.o: $(APP)/src/%.c
	mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

LINKER_FILE=linker_script.ld
LDFLAGS=-T $(LINKER_FILE) $(NEWLIB_NANO)

mhs.elf: $(APP_O) $(PLATFORM_O) $(ROBOS_LIB)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(APP_O) $(PLATFORM_O) $(ROBOS_LIB) -o mhs.elf