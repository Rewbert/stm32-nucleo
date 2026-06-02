# Bare metal STM32L552ZEQ with TrustZone-M

This repository contains my experiments with STM32 Nucleo board(s). I wanted to learn more bare metal programming, and what better way than not just to program a devkit, but also add TrustZone to the mix? (_foreshadowing_: it is a nightmare).

The project defines its own linker script(s), bootloader(s), library code to access device peripherals, and build system (collection of Makefiles). The project does not use STMCubeIDE, or ST HAL. The only external project I depend on is the ST CMSIS project, which implements header files that define the address structure of the board. I have included the relevant files from CMSIS in this repo. When it comes to programming the board, I have pieced information together from sources online, and read the reference manual a thousand times.

Initially I started hacking using a STM32L552ZEQ Nucleo-144 board that I had. I have since bought a STM32U5A5ZJ (much more memory), but have yet to program it. I have, in preparation for this, reimplemented my libraries in a driver-style, such that application code hopefully should not have to be modified when compiling for the two different boards.

NOTE: The repository contains a lot of old artifacts right now that are obsolete, but which I keep around for documentation. I might purge them at a later stage. If something in the repo is not mentioned in this README, it is safe to assume that it is one of these 'archived' files/folders.

---

##### Requirements

* The STM32L552ZEQ Nucleo-144 board (MB1361)
* The arm cross-compiler toolchain, `arm-none-eabi`
* Programmer (I used OpenOCD)
* `make`

---

##### Project structure

* `vendor/CMSIS/` -- Common Microcontroller Software Interface Standard. Defines the register layouts and memory-mapped addresses for the chip. I've downloaded the relevant headers from the CMSIS GitHub and included them here. I am unsure of the division of ownership, but there are ST owned repos that include headers for these boards.
* `firmware/drivers/` -- A driver library written with TrustZone awareness in mind. Each driver compiles  for both the secure and non-secure worlds depending on whether `-DSECURE` is passed. Covers GPIO, UART (LPUART1), RCC, PWR, EXTI, SysTick, Flash, MPCBB, and SAU. It is vastly incomplete, and will still undergo major changes as I experiment further with TrustZone. The supported peripherals are those I've needed, and no more.
* `firmware/boards/` -- Board abstraction. `board.h` defines a board-independent API: `board_led()`, `board_console()`, `board_rcc()`, `board_button_exti()`, and so on. The implementation in `firmware/boards/stm32l5/board.c` wires these up. The idea is that adding a second board (e.g. STM32U5) should only require a new implementation file here.
* `firmware/bootloader/` -- The bootloader(s) for the secure and nonsecure world. It runs before the application and sets up the memory protection (via the MPCBB driver), which partitions SRAM1 and SRAM2 between the secure and non-secure worlds. The chip-specific initialisation lives in `firmware/bootloader/stm32l5/S/src/tz_init.c`.
* `examples/` -- Example applications using `firmware/drivers/` and the board abstraction. Each example has its own Makefile and builds a secure/non-secure ELF pair.
* `mk-drivers/` -- Makefile includes. `mk-drivers/toolchain.mk` defines the toolchain flags. `mk-drivers/` contains reusable build rules for the driver library, bootloader, and board, which any application makefile can include.

---

##### The driver library

`firmware/drivers/` is an important part of the project. The drivers are designed to be portable across security domains. Each driver is compiled twice when building a TrustZone application: once with `-DSECURE` (producing `build/tz-lib-drivers-secure.a`) and once without (producing `build/tz-lib-drivers-nonsecure.a`). The `domain/domain.h` header takes care of selecting the right CMSIS headers and providing the `NONSECURE_CALLABLE` annotation for NSC functions.

The idea is that every peripheral is compiled for both domains, but with security-aware functionality chucked away during compile time by the C preprocessor if we compile for the nonsecure world. I have implemented the parts of the peripherals that I currently use in my project, and will extend them with more of their functionality as I go along.

I have come to understand that there (obviously) is a distinction between the CPU and MCU. The CPU is the Arm Cortex-M33, whereas the MCU is everything else (e.g. peripherals added by STM). A consequence of this is that some peripherals belong to the CPU, and some to the MCU. As an example, the `sau` (software attribution unit) is a CPU peripheral, whereas the `mpcbb` (peripheral that configures TZ-boundaries for the SRAM) is a MCU peripheral. The `sau` driver should be board agnostic, as long as the other boards all use the same CPU.

The drivers currently available are:

* `gpio` — configure and toggle GPIO pins
* `uart` — LPUART1 send/receive
* `rcc` — clock enable and PLL configuration
* `pwr` — power control (e.g. enabling VDDIO2, needed for LPUART1 on GPIOG)
* `exti` — external interrupt configuration and callback registration
* `systick` — millisecond delay
* `flash` — flash wait state configuration (needed before raising the clock)
* `mpcbb` — Memory Protection Controller for Block-Based memory, used to partition SRAM between the two worlds
* `sau` — Security Attribution Unit, used to mark memory regions as secure or non-secure

---

##### The board abstraction

Some information an application developer might be interested in is accessing their boards LEDs, Buttons, etc. There is a board abstraction for this. There are named entities such as `BOARD_LED_GREEN` that can be used by an application to refer to a board LED, and it is then up to the board implementation to correctly map that name to the correct `gpio`. The board abstraction additionally does some setup and configuration. It is intended to initialise many drivers, and configure e.g. the `uart` backends `gpio`s.

This makes it straightforward to write an application once and port it to a second board by swapping the board implementation.

---

##### TrustZone

The TrustZone setup follows the standard Arm TrustZone-M model: the secure world starts first, configures the SAU and MPCBB to partition memory, runs secure `main`, and then hands off to the non-secure world. Secure functions that the non-secure world is allowed to call are marked `NONSECURE_CALLABLE `(which expands to `__attribute__((cmse_nonsecure_entry))`). The linker produces a CMSE import library (`build/secure_cmse_import.lib`) alongside the secure ELF, which the non-secure application links against.

---

##### How to build

The small blink-and-button example using `firmware/drivers/` and the board abstraction is built with:

```bash
make -f examples/blink-and-button/Makefile all
```

This produces `secure.elf`. To flash it:

```bash
make flash FILE=secure.elf
```

To debug, issue `make openocd` in one terminal and `make debug FILE=<elf>` in another.

---

##### Documents

* Reference manual: https://www.st.com/resource/en/reference_manual/dm00346336-stm32l552xx-and-stm32l562xx-advanced-arm-based-32-bit-mcus-stmicroelectronics.pdf
* Board user manual: https://www.st.com/resource/en/user_manual/um2581-stm32l5-nucleo144-board-mb1361-stmicroelectronics.pdf
* Arm TrustZone-M: https://www.arm.com/technologies/trustzone-for-cortex-m
