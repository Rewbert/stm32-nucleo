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

CPPFLAGS += -I$(ROBOS_INC)

build/$(ROBOS)/%.o: $(ROBOS)/src/%.c
	mkdir -p $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

$(ROBOS_LIB): $(ROBOS_OBJ)
	arm-none-eabi-ar rcs $@ $^