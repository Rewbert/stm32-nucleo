# Some experiments with TrustZone enabled STM32L552ZE_Q, bare metal.

Currently, no TrustZone is used. I am working on that and will push that at a later date. This is a bare metal project, so STMCUBE IDE is not used.

##### Requirements

* The board
* arm-none-eabi toolchain
* some USB cable to connect the STLINK to your computer

##### Project structure

* `CMSIS/` "Common Microcontroller Software Interface Standard" declares all the `#define`'s required to more easily access the registers and stuff on the board.
* `robos/` My support files, including system calls, startup script, code to configure LPUART, PLL, and install SysTick timer. The code configures the clock to run at the faster 110 MHz, LPUART1 to enable and transmit over the STLINK/V2 with a baud rate of 115200, and installs a SysTick timer with millisecond precision.
* `mhs/` MicroHaskell target. Uses an older version of the MHS RTS (used in another project of mine), but should be easilly replaced with a newer version (Lennart, you can do this, or perhaps add another build target that builds the newest RTS?)
* `main/` A simple, minimal, application. Showcases everything implemented so far in terms of uart etc.
* `Makefile` Specifies rules for building, flashing, and debugging
* `linker_Script.ld` Manual linker script

##### How to build

The Makefile has two targets, `mhs` or `main`. Otherwise, you can issue

* `make TARGET=<target>`
* `make TARGET=<target> flash`
* `make TARGET=<target> debug`
* `make openocd`

To debug the board using GDB, issue `make openocd` in one terminal and `make TARGET=<target> debug` in another terminal.

##### Documents

* Reference manual for the processor: https://www.st.com/resource/en/reference_manual/dm00346336-stm32l552xx-and-stm32l562xx-advanced-arm-based-32-bit-mcus-stmicroelectronics.pdf
* User manual for the board: https://www.st.com/resource/en/user_manual/um2581-stm32l5-nucleo144-board-mb1361-stmicroelectronics.pdf
