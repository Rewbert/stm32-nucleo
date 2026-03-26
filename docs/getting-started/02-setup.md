# 2. Setup

## Hardware Required

A STM32L5 or STM32U5 board, as well as a USB cable to connect to the ST-LINK.

## Toolchain

### arm-none-eabi

This is released by ARM, and contains tools such as GCC, GDB, objdump, etc. It must be on your `$PATH`.

### OpenOCD

We use _OpenOCD_ to flash the firmware onto the boards. For STM32L5, `apt` was enough to install an appropriate version, but for STM32U5 it was not. ST have their own fork of OpenOCD where they add support for their boards, and eventually merge these changes to upstream OpenOCD. For STM32U5, support has not yet been upstreamed.

**I had to clone and manually build openocd myself**, from ST's fork, to be able to flash the STM32U5. Here is the repo: https://github.com/STMicroelectronics/OpenOCD

I've cloned it in my $(HOME), built it, and then refer to it in the top-level makefile in the root of this project. Please look at the Makefile to see how it is referred to.

### make

`make` is required to build the firmware, but there is nothing special to say about make. Any version should suffice.

## Cloning the Repository

Just clone this repo and you are good to go. If you want to build the MicroHaskell projects, you have to also clone MicroHs (https://github.com/augustss/MicroHs) in the root of this repository. The makefiles for the Haskell projects refer to the runtime system relative to the root of this repository.
