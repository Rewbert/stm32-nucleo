include mk-drivers/board.mk
include mk-drivers/ssm.mk

###
# Build the signal-generator example: a single secure-only application using the
# firmware/drivers and firmware/boards framework, plus the SSM runtime (out-of-tree,
# see SSM_DIR in mk-drivers/ssm.mk).
###

APP := examples/signal-generator

SRC := $(APP)/main.c
OBJ := $(patsubst %.c, build/s/%.o, $(SRC))

build/s/$(APP)/%.o: $(APP)/%.c
	mkdir -p $(dir $@)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SSM_INC) -DNDEBUG -c $< -o $@

.PHONY: all
all: secure.elf

secure.elf: $(SECURE_BOOT_O) $(BOARD_O) $(OBJ) $(SSM_NONSECURE_A) $(TZ_LIB_DRV_SECURE_A)
	$(CC) $(TZ_CFLAGS) $(SECURE_CPPFLAGS) $(SECURE_LDFLAGS) -o $@ $^
