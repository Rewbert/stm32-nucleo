ifndef MK_DRIVERS_SSM
MK_DRIVERS_SSM := 1

include mk-drivers/toolchain.mk

ifeq ($(origin SSM_DIR),undefined)
$(error SSM_DIR is not set -- clone https://github.com/Rewbert/ssm-runtime and export SSM_DIR to point at it, e.g. export SSM_DIR=$$HOME/Projects/ssm-runtime)
endif

SSM_SRC := $(wildcard $(SSM_DIR)/src/*.c)

ifeq ($(SSM_SRC),)
$(error No SSM sources found under SSM_DIR=$(SSM_DIR) -- check that it points at a valid ssm-runtime checkout)
endif

SSM_INC      := -I$(SSM_DIR)/include
SSM_CFLAGS   := $(CFLAGS) $(TRUSTZONE_AWARENESS) $(NO_STDLIB) $(NOSTARTFILES) $(DEBUG)

SSM_CPPFLAGS := $(CPPFLAGS) $(SSM_INC) -DNDEBUG

SSM_NONSECURE_A := build/ssm-nonsecure.a
SSM_NONSECURE_O := $(patsubst $(SSM_DIR)/%.c,build/ns/$(SSM_DIR)/%.o,$(SSM_SRC))

build/ns/$(SSM_DIR)/%.o: $(SSM_DIR)/%.c
	mkdir -p $(dir $@)
	$(CC) $(SSM_CFLAGS) $(SSM_CPPFLAGS) -c $< -o $@

$(SSM_NONSECURE_A): $(SSM_NONSECURE_O)
	$(AR) rcs $@ $^

endif
