# 3. Build and Flash

## Building the Example Apps

The makefiles will assume you build for the STM32L5 board, unless you specify that you want to build the firmware for STM32U5 via the BOARD variable. The example applications in this repository are built with

- `examples/blink-and-button/` - built with `make -f examples/blink-and-button/Makefile BOARD=stm32u5 all`
- `examples/breadboard-buttons/` - built with `make -f examples/breadboard-buttons/Makefile BOARD=stm32u5 all`
- `examples/microhs-hello-tz/` - built with `make -f examples/microhs-hello-tz/Makefile BOARD=stm32u5 all`
- `examples/microhastee/` - built with `make -f examples/microhastee/Makefile BOARD=stm32u5 all`

All of these targets will build two artifacts -- a `secure.elf` and `nonsecure.elf`.

## Flashing a Single ELF

`make flash BOARD=stm32u5 FILE=<your elf>`

## Flashing a TrustZone Pair

`make flash_tz BOARD=stm32u5 SECURE=<your secure elf> NONSECURE=<your nonsecure elf>`

**NOTE**: This will produce some error about checksums. Your program will flash successfully anyway, but openocd will try to verify that things were flashed correctly, and run into problems reading the FLASH from secure world (or something like that). I am sure it is fixable, but I do not know how. I just ignore this.

## Debugging with GDB

1. In one terminal - `make BOARD=stm32u5 openocd`
2. In another terminal - `make debug_tz BOARD=stm32u5 SECURE=<your secure elf> NONSECURE=<your nonsecure elf>`

Now you are connected to the device in GDB, and can step through your program. The examples are all built with the debug flag, so there should be symbols for everything.

## Connecting a Console (UART)

`minicom -D /dev/ttyACM0` (for me, make sure you use the right /dev for you)
