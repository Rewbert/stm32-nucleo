# Framework Documentation

## What This Is

This repository documents my experiments with writing bare metal code for the STM32L5 and STM32U5 Nucleo boards. These boards are interesting as they come with an ARM TrustZone-equipped Cortex-M33 CPU. In this repository, I program the TrustZone from scratch, implementing my own HAL.

This specific document tries to document how to use the code found here. The repository also contains older code from previous experiments, which I will remove at some point. I suggest another user of the code found in this repository to not rely on or use code in directories that are not documented in this document.

**NOTE**: I want to emphasize that while the code here works perfectly fine, it is not _complete_ in the sense that you can program the entire devices using this codebase. The library described here supports that functionality which I've experimented with in my own projects, but does not support anything else. I have tried to write the code with a clear structure in mind, however, so that if anyone wants to contribute with drivers for more peripherals (or add more support to the drivers already existing), they can do so. Please open PRs :) .

**NOTE 2**: The code here was written while learning bare-metal and TrustZone programming, so there are likely places where a more experienced embedded developer would do things differently. I have tried to be deliberate about the structure and design, but I make no claim that it is optimal. Suggestions welcome.

**NOTE 3**: The purpose of the project is to implement code that can be executed on this family of devices (the Nucleo boards). I believe the code here might be useful for/ported to other devices in the Nucleo family of boards, but I have only tested it on the two boards specifically mentioned in this document. While I aim to keep the library somewhat suitable for this family of devices, I have no intention of making the project generic enough to support other families of boards (e.g. NRF52). I believe that would make the project spiral out of control.

## Who This Is For

This code is for anyone who wants to write TrustZone applications on STM32 Nucleo hardware, whether as a learning exercise or as a starting point for a real project. No prior experience programming TrustZone is assumed -- familiarity with the concept is enough.

## Why This Exists

STMicroElectronics provides tools to program their devices, but they can feel heavy, and using them obscures what is actually happening on the hardware. The purpose of this project, for me, was to learn bare-metal and TrustZone programming from the ground up. The only external dependency is CMSIS, which provides register definitions for the hardware.

## How to Navigate

The key directories are

* `TZ-drivers-bootloader/` Two bootloaders, one per board. Each maintains a vector table, implements the reset handler, and configures the TrustZone security policy (SAU/MPCBB). The secure bootloader then hands off to the secure applications `main`, and once that returns, hands off to the non-secure `main`.
* `TZ-lib-drivers/` The TrustZone-aware HAL. Each peripheral is implemented as a device driver, with board-specific implementations where needed. Peripherals that belong to the CPU (rather than the MCU) are implemented once, shared across both boards.
* `boards/` The board abstraction layer. Maps board-specific details such as which pin is which LED, where the buttons are, onto the driver interfaces in `TZ-lib-drivers/`.
* `manuals/` Contains the reference manuals and user manuals for the STM32L5 and STM32U5 families of boards.

Example applications

* `TZ-drivers-MicroHaskell/` Compiles and runs two instances of MHS, one in the secure application and one in the nonsecure. They just print and play with the LEDs, and do not communicate with each other. **NOTE**: For this to work, you need to clone MicroHs into the root of the project. Look at the makefile in `TZ-drivers-MicroHaskell/` to see how it looks for the mhs runtime sources. Build with `make -f TZ-drivers-MicroHaskell/Makefile BOARD=[stm32l5|stm32u5] all`
* `TZ-drivers-HasTEE/` This is my own main project. It runs two MHS instances, much like the above mentioned project, but there is also a communication path between them. The Haskell code that goes together with this project can be found in `HaskellApp/` (perhaps this should be moved to be within `TZ-drivers-HasTEE/`...). Build with `make -f TZ-drivers-HasTEE/Makefile BOARD=[stm32l5|stm32u5] all`
* `TZ-drivers-example/` This little project shows how I might perhaps structure a project that has a breadboard with some buttons hooked up, as well as how configuring those buttons. Not a super interesting example, but here we are. Build with `make -f tz-drivers-elf.mk BOARD=[stm32l5|stm32u5] all`
* `main-drivers/` and `ns-main-drivers/` are the secure and nonsecure applications of yet another small example. It configures UART, the on board button, the LEDs, etc. Build with `make -f elf.mk BOARD=[stm32l5|stm32u5] all`.

**TODO** (move the building and flashing crap to appropriate sections later, but leave here for Lennart to find for the moment)

**IMPORTANT**: Before the trustzone code here can be flashed into either board, some option bytes have to be modified. Namely, the TZEN bit has to be set to 1, and two of the watermark registers have to be configured to mark on of the two banks as nonsecure. By default, all the memory is secure, so we need to modify that. This is a persistent change that _you only need to do once_. To do this we change `SECWM2_PSTRT=0x1` and `SECWM2_PEND=0x0`. This marks the entire second bank as nonsecure (which is right for us, given that we do a 50/50 split of the memory). **do not change the RDP crap**, you may end up bricking your board.

Flash with `make flash_tz SECURE=<your secure elf> NONSECURE=<your nonsecure elf>`. You will get some checksum error afterwards, but you can disregard that (or tell me how to fix it).

Debug with

1. Terminal one, `make BOARD=[stm32l5|stm32u5] openocd`
2. Terminal two, `make debug_tz SECURE=<your secure elf> NONSECURE=<your nonsecure elf>`. This opens GDB

**NOTE**: I had to clone and manually build **openocd** myself, from ST's fork, to be able to flash the STM32U5. They have not managed to upstream their STM32U5 support into mainline openocd yet. Here is the repo: https://github.com/STMicroelectronics/OpenOCD

I've cloned it in my $(HOME), built it, and then refer to it in the top-level makefile in the root of this project.

### Getting Started

1. [Setup](getting-started/01-setup.md)
2. [Build and Flash](getting-started/02-build-and-flash.md)
3. [Hello World](getting-started/03-hello-world.md)

### How-To Guides

- [Add a Custom GPIO Pin](how-to/custom-gpio-pin.md)
- [Register a Button Interrupt](how-to/button-interrupt.md)
- [Configure the System Clock](how-to/configure-clock.md)
- [Build a TrustZone Application](how-to/trustzone-build.md)
- [Expose a Function to the Non-Secure World](how-to/nonsecure-callable.md)

### API Reference

- [board.h](reference/board.md) — Primary entry point
- [gpio.h](reference/gpio.md)
- [uart.h](reference/uart.md)
- [exti.h](reference/exti.md)
- [systick.h](reference/systick.md)
- [rcc.h](reference/rcc.md)
- [pwr.h / flash.h](reference/pwr-flash.md)
- [TrustZone (SAU, MPCBB, TZSC, domain.h)](reference/trustzone.md)
- [irq.h](reference/irq.md)

### Concepts

- [TrustZone](concepts/trustzone-init-sequence.md)
- [Architecture Overview](concepts/architecture.md)
- [The Vtable / Opaque Backend Pattern](concepts/vtable-backend-pattern.md)
- [The HAL_SECURE Compilation Model](concepts/hal-secure-model.md)
