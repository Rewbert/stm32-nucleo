ENTRY(reset_handler)

/* STM32U5A5ZJQ — non-secure region: second 2 MB of flash, 1248 KB contiguous SRAM.
 * Flash NS alias starts at 0x08200000 (= 0x08000000 + 2 MB).
 * NS SRAM: SRAM3[SB 26-51] (416 KB) + SRAM5 (832 KB), NS-bus alias 0x20138000–0x2026FFFF. */

MEMORY
{
    ISR   (rw):  ORIGIN = 0x08200000, LENGTH = 2K    /* ISR vector table (512 words) */
    FLASH (rx):  ORIGIN = 0x08200800, LENGTH = 2046K /* 2 MB − 2K ISR */
    SRAM  (rwx): ORIGIN = 0x20138000, LENGTH = 1248K /* SRAM3[SB 26-51] + SRAM5 */
}

PROVIDE(_estack = ORIGIN(SRAM) + LENGTH(SRAM) - 4);

SECTIONS
{
    .isr_vector :
    {
        KEEP(*(.isr_vector))
    } > ISR

    .text :
    {
        . = ALIGN(4);
        *(.text)
        *(.text.*)

        __exidx_start = .;
        KEEP(*(.ARM.exidx*))
        __exidx_end = .;

        . = ALIGN(4);
        _etext = .;
    } > FLASH

    .rodata :
    {
        . = ALIGN(4);
        *(.rodata)
        *(.rodata.*)
        . = ALIGN(4);
    } > FLASH

    .data :
    {
        . = ALIGN(4);
        _sdata = .;
        *(.data)
        *(.data.*)
        . = ALIGN(4);
        _edata = .;
    } > SRAM AT> FLASH

    _sidata = LOADADDR(.data);

    .bss :
    {
        . = ALIGN(4);
        _sbss = .;
        __bss_start__ = _sbss;
        *(.bss)
        *(.bss.*)
        *(COMMON)
        . = ALIGN(4);
        _ebss = .;
        __bss_end__ = _ebss;
    } > SRAM

    . = ALIGN(8);
    _end = .;
    PROVIDE(end = .);
}
