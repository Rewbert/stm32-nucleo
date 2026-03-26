# 3. Build and Flash

## Building the Example Apps

The makefiles will assume you build for the STM32L5 board, unless you specify that you want to build the firmware for STM32U5 via the BOARD variable. The example applications in this repository are built with

- `main-drivers/` & `ns-main-drivers/` - built with `make -f elf.mk BOARD=stm32u5 all`
- `TZ-drivers-example/` - built with `make -f tz-drivers-elf.mk BOARD=stm32u5 all`
- `TZ-drivers-MicroHaskell/` - built with `make -f TZ-drivers-MicroHaskell/Makefile BOARD=stm32u5 all`
- `TZ-drivers-HasTEE/` - built with `make -f TZ-drivers-HasTEE/Makefile BOARD=stm32u5 all`

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