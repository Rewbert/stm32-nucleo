include mk/common/platform.mk

# variables

MHS := MicroHs
MHS_DIR := MicroHs/src/runtime
MHS_SRC := \
  $(MHS_DIR)/eval.c \
  new-mhs/main.c \
  new-mhs/config.c \
  $(MHS)/gen.c

MHS_INC = -Inew-mhs -IMicroHs/src/runtime # I$(MHS_DIR)

MHS_O := $(patsubst %.c,build/new-mhs/%.o,$(MHS_SRC))

NEW_MHS_CPPFLAGS = $(CPPFLAGS) $(MHS_INC) -I$(ROBOS_INC)
NEW_MHS_CFLAGS = $(CFLAGS) $(DEBUG)

NEW_MHS_LINKER_FILE=linker_script.ld
NEW_MHS_LDFLAGS=-T $(NEW_MHS_LINKER_FILE) $(NEWLIB_NANO)

# rules

build/new-mhs/MicroHs/src/runtime/%.o: MicroHs/src/runtime/%.c
	mkdir -p $(dir $@)
	$(CC) $(NEW_MHS_CPPFLAGS) $(NEW_MHS_CFLAGS) -c $< -o $@

build/new-mhs/MicroHs/%.o: MicroHs/%.c
	mkdir -p $(dir $@)
	$(CC) $(NEW_MHS_CPPFLAGS) $(NEW_MHS_CFLAGS) -c $< -o $@

build/new-mhs/new-mhs/%.o: new-mhs/%.c
	mkdir -p $(dir $@)
	$(CC) $(NEW_MHS_CPPFLAGS) $(NEW_MHS_CFLAGS) -c $< -o $@

build/new-mhs/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(NEW_MHS_CPPFLAGS) $(NEW_MHS_CFLAGS) -c $< -o $@

$(MHS_O): build/new-mhs/%.o : %.c
	mkdir -p $(dir $@)
	$(CC) $(NEW_MHS_CPPFLAGS) $(NEW_MHS_CFLAGS) -c $< -o $@


# final build target

newmhs.elf: $(MHS_O) $(PLATFORM_O) $(ROBOS_LIB)
	$(CC) $(NEW_MHS_CPPFLAGS) $(NEW_MHS_CFLAGS) $(NEW_MHS_LDFLAGS) $(MHS_O) $(PLATFORM_O) $(ROBOS_LIB) -o newmhs.elf