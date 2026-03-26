# Preparing the Board for TrustZone

A factory new board comes with TrustZone disabled by default. If you want to use TrustZone, you have to make two modifications to the board (both controlled via option bytes).

First, the `TZEN` bit has to be set to `0x1`, and then parts of the second bank of FLASH has to be marked as nonsecure. If you use the bootloader I've written, we need to mark the entire second bank of FLASH as nonsecure, as the bootloader does a 50/50 split of the memory between the secure and nonsecure application.

There are watermarks that we can configure to achieve this. The watermarks define a range of pages in a bank that are considered secure, and by setting the start value to be larger than the end value, we mark the whole thing as nonsecure. You need to modify `SECWM2_PSTRT=0x1` and `SECWM2_PEND=0x0`.

I do this with `STM32_Programmer_CLI`. I do not remember how I installed it, but I believe it came with some ST software (the STMCubeIDE perhaps?). I am sure searching online will yield results. The command that I ran was
```
STM32_Programmer_CLI -c port=SWD -ob TZEN=1 SECWM2_PSTRT=0x1 SECWM2_PEND=0x0
```

You can read the option bytes back with
```
STM32_Programmer_CLI -c port=SWD -ob displ
```
The top-level makefile defines this, so you can just do `make read_option_bytes`.

I had to do these modifications to both the STM32L5 and STM32U5.

**NOTE**: Do not try to change the `RDP` crap. If you set it to the wrong level, you will brick your device.

# Using the Library Without TrustZone

The secure code boots into the reset vector found at a specific found in memory, and it just so happens that a board with `TZEN=0x0` will by default boot into the reset vector found at the same address. Since there are no privileged operations, this non-TrustZone application should not encounter any errors when trying to configure security, as these operations are not protected anymore.

When building the example applications, it should be possible to just flash the secure `elf`-file to the board, and boot from there. You will need a dummy nonsecure-callable function in your program, or else the Makefile will complain when trying to generate the library file.

I do believe it makes most sense to configure the board for TrustZone, however, as that is how the library is intended to be used.