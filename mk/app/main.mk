include mk/common/platform.mk

SOURCE := main
SOURCE_C := $(SOURCE)/main.c

SOURCE_O := $(patsubst $(SOURCE)/%.c,build/app/%.o, $(SOURCE_C))

CFLAGS += $(DEBUG)

build/app/%.o: main/%.c
	mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

LINKER_FILE=linker_script.ld
LDFLAGS=-T $(LINKER_FILE) $(NEWLIB_NANO)

main.elf: $(SOURCE_O) $(PLATFORM_O) $(ROBOS_LIB)
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(SOURCE_O) $(PLATFORM_O) $(ROBOS_LIB) -o main.elf