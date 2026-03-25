# TrustZone

The notes in this document all refer to TrustZone-M, the variant for microcontrollers. There is also TrutsZone-A for larger machines, but we do not concern ourselves with that.

## TrustZone as a Concept

TrustZone is a mechanism that allows for an application to designate code, data, and resources (e.g. peripherals) as privileged and protected. What this means is that the unprivileged part of the application cannot access or modify them. The idea is that you store your valuable data or control your critical peripherals only from the secure application, and keep the rest of your application running in unprivileged mode.

The programming model is that of a client-server relationship. The secure application initially performs setup, configures peripherals etc, and then hands over execution to the nonsecure application. The nonsecure application can then make calls to special procedures in the secure application, called nonsecure-callable. The secure application places nonsecure-callable procedures in a special segment which has been configured such that the nonsecure application may invoke them. **Note**: _only_ those procedures in the nonsecure-callable region can be invoked by the nonsecure application. The existence of other procedures in the secure application is completely unknown by the nonsecure application.

In practice, the memory on your device (both FLASH and RAM) is divided into two parts, one for the secure application and one for the nonsecure. There is only one contiguous region of RAM, but it is memory mapped into two different address spaces. The secure application initially owns a lot of resources, but can declassify some (or all) of them so that the nonsecure application may use them. There are two `main` procedures, one for each application. The build system will construct and flash two `.elf`-files, one for each memory partition.

## Boot Flow

1. After reset, the secure bootloader runs (the reset handler from the secure memory is invoked)
2. The reset handler does the usual reset-handler'y things such as copying data from FLASH to RAM, and then goes on to configure the SAU (Software Attribution Unit)
3. After this, the MPCBB is configured, deciding which segments of SRAM is secure and which is nonsecure
4. Secure `main()` executes
5. Once secure `main` returns, the secure bootloader invokes the nonsecure `main()`

## SAU: Defining Memory Regions

## MPCBB: Partitioning SRAM

## Watermark things -- TODO RENAME WHEN I FILL THIS IN

## Handing Off to the Non-Secure World

## Differences Between STM32L5 and STM32U5
