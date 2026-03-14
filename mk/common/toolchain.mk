CC      := arm-none-eabi-gcc
GDB     := arm-none-eabi-gdb
OBJCOPY := arm-none-eabi-objcopy
AR      := arm-none-eabi-ar

TRUSTZONE_AWARENESS := -mcmse
NEWLIB_NANO := --specs=nano.specs
NO_STDLIB := --specs=nosys.specs
NOSTARTFILES := -nostartfiles
CPUFLAGS := -mcpu=cortex-m33 -mthumb

# Select MCU-specific defines and CMSIS paths based on BOARD.
# Default to stm32l5; pass BOARD=stm32u5 on the command line for the U5 target.
BOARD ?= stm32l5

ifeq ($(BOARD),stm32u5)
COMMON_DEFS := -DSTM32U5A5xx
CMSIS_INC := \
  -ICMSIS/Device/ST/STM32U5/Include \
  -ICMSIS/CMSIS/Core/Include
else
COMMON_DEFS := -DSTM32L552xx
CMSIS_INC := \
  -ICMSIS/Device/ST/STM32L5/Include \
  -ICMSIS/CMSIS/Core/Include
endif

CPPFLAGS := $(COMMON_DEFS) $(CMSIS_INC)
CFLAGS := $(CPUFLAGS)

DEBUG := -g # change this later to remove the debug table
