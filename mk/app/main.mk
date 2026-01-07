include mk/common/platform.mk

SOURCE := main
SOURCE_C := $(SOURCE)/main.c

SOURCE_O := $(patsubst $(SOURCE)/%.c,build/app/%.o, $(SOURCE_C))

MAIN_CFLAGS = $(CFLAGS) $(DEBUG)
MAIN_CPPFLAGS = $(CPPFLAGS) -I$(ROBOS_INC)

build/app/%.o: main/%.c
	mkdir -p $(dir $@)
	$(CC) $(MAIN_CPPFLAGS) $(MAIN_CFLAGS) -c $< -o $@

MAIN_LINKER_FILE=linker_script.ld
MAIN_LDFLAGS=-T $(MAIN_LINKER_FILE) $(NEWLIB_NANO)

main.elf: $(SOURCE_O) $(PLATFORM_O) $(ROBOS_LIB)
	$(CC) $(MAIN_CPPFLAGS) $(MAIN_CFLAGS) $(MAIN_LDFLAGS) $(SOURCE_O) $(PLATFORM_O) $(ROBOS_LIB) -o main.elf