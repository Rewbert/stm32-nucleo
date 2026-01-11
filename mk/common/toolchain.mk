CC      := arm-none-eabi-gcc
GDB     := arm-none-eabi-gdb
OBJCOPY := arm-none-eabi-objcopy
AR      := arm-none-eabi-ar

TRUSTZONE_AWARENESS := -mcmse
NEWLIB_NANO := --specs=nano.specs
NO_STDLIB := --specs=nosys.specs
NOSTARTFILES := -nostartfiles
CPUFLAGS := -mcpu=cortex-m33 -mthumb

COMMON_DEFS := -DSTM32L552xx
CMSIS_INC := \
  -ICMSIS/Device/ST/STM32L5/Include \
  -ICMSIS/CMSIS/Core/Include

CPPFLAGS := $(COMMON_DEFS) $(CMSIS_INC)
CFLAGS := $(CPUFLAGS)

DEBUG := -g # change this later to remove the debug table