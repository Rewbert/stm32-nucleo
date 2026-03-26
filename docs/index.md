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

* `TZ-drivers-MicroHaskell/` Compiles and runs two instances of MHS, one in the secure application and one in the nonsecure. They just print and play with the LEDs, and do not communicate with each other. **NOTE**: For this to work, you need to clone MicroHs into the root of the project. Look at the makefile in `TZ-drivers-MicroHaskell/` to see how it looks for the mhs runtime sources.
* `TZ-drivers-HasTEE/` This is my own main project. It runs two MHS instances, much like the above mentioned project, but there is also a communication path between them. The Haskell code that goes together with this project can be found in `HaskellApp/` (perhaps this should be moved to be within `TZ-drivers-HasTEE/`...).
* `TZ-drivers-example/` This little project shows how I might perhaps structure a project that has a breadboard with some buttons hooked up, as well as how configuring those buttons. Not a super interesting example, but here we are.
* `main-drivers/` and `ns-main-drivers/` are the secure and nonsecure applications of yet another small example. It configures UART, the on board button, the LEDs, etc.

### Getting Started

1. [Preparing the Board](getting-started/01-preparing-the-board.md) <- this has text, read here
2. [Setup](getting-started/02-setup.md) <- this has text, red here
3. [Build and Flash](getting-started/03-build-and-flash.md) <- this has text, read here
4. [Hello World](getting-started/04-hello-world.md)

### How-To Guides

- [Add a Custom GPIO Pin](how-to/custom-gpio-pin.md)
- [Register a Button Interrupt](how-to/button-interrupt.md)
- [Configure the System Clock](how-to/configure-clock.md)
- [Build a TrustZone Application](how-to/trustzone-build.md)
- [Expose a Function to the Non-Secure World](how-to/nonsecure-callable.md)

### Library API Reference

- [library design](reference/library-design.md) <- this has text, read here
- [board.h](reference/board.md)
- [gpio.h](reference/gpio.md)
- [uart.h](reference/uart.md)
- [exti.h](reference/exti.md)
- [systick.h](reference/systick.md)
- [rcc.h](reference/rcc.md)
- [pwr.h / flash.h](reference/pwr-flash.md)
- [TrustZone (SAU, MPCBB, TZSC, domain.h)](reference/trustzone.md)
- [irq.h](reference/irq.md)

### Concepts

- [TrustZone](concepts/trustzone-init-sequence.md) <- this has text, read here
- [Architecture Overview](concepts/architecture.md)
- [The Vtable / Opaque Backend Pattern](concepts/vtable-backend-pattern.md)
- [The HAL_SECURE Compilation Model](concepts/hal-secure-model.md)
