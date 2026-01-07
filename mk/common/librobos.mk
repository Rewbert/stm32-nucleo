# both main.mk and mhs.mk includes this, so we guard it. Otherwise we get warnings that
# we're redefining the rule for build/librobos.a
ifndef LIBROBOS
LIBROBOS := 1

include mk/common/toolchain.mk

ROBOS := robos
ROBOS_SRC := \
  $(ROBOS)/src/clock.c \
  $(ROBOS)/src/uart.c \
  $(ROBOS)/src/gpio.c \
  $(ROBOS)/src/timer.c \
  $(ROBOS)/src/button.c
ROBOS_INC = $(ROBOS)/include

ROBOS_OBJ := $(patsubst $(ROBOS)/src/%.c,build/$(ROBOS)/%.o,$(ROBOS_SRC))
ROBOS_LIB := build/librobos.a

LIBROBOS_CPPFLAGS = $(CPPFLAGS) -I$(ROBOS_INC)

build/$(ROBOS)/%.o: $(ROBOS)/src/%.c
	mkdir -p $(dir $@)
	$(CC) $(LIBROBOS_CPPFLAGS) $(CFLAGS) -c $< -o $@

$(ROBOS_LIB): $(ROBOS_OBJ)
	arm-none-eabi-ar rcs $@ $^

endif