include mk/common/platform.mk

APP := mhs
APP_SRC := \
  $(APP)/src/eval_stm32l5.c \
  $(APP)/src/gen.c

APP_O := $(patsubst $(APP)/src/%.c,build/app/$(APP)/%.o,$(APP_SRC))

APP_INC := $(APP)/include

MHS_CPPFLAGS = $(CPPFLAGS) -I$(APP_INC) -I$(ROBOS_INC)
MHS_CFLAGS = $(CFLAGS) $(DEBUG)

build/app/$(APP)/%.o: $(APP)/src/%.c
	mkdir -p $(dir $@)
	$(CC) $(MHS_CPPFLAGS) $(MHS_CFLAGS) -c $< -o $@

MHS_LINKER_FILE=linker_script.ld
MHS_LDFLAGS=-T $(MHS_LINKER_FILE) $(NEWLIB_NANO)

mhs.elf: $(APP_O) $(PLATFORM_O) $(ROBOS_LIB)
	$(CC) $(MHS_CPPFLAGS) $(MHS_CFLAGS) $(MHS_LDFLAGS) $(APP_O) $(PLATFORM_O) $(ROBOS_LIB) -o mhs.elf