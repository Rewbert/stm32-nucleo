ENTRY(reset_handler)

/* STM32U5A5ZJQ — non-secure region: second 2 MB of flash, second 384 KB of SRAM1.
 * Flash NS alias starts at 0x08200000 (= 0x08000000 + 2 MB).
 * SRAM1 NS alias for superblocks 24-47 starts at 0x20060000 (= 0x20000000 + 24 × 16 KB). */

MEMORY
{
    ISR   (rw):  ORIGIN = 0x08200000, LENGTH = 2K    /* ISR vector table (512 words) */
    FLASH (rx):  ORIGIN = 0x08200800, LENGTH = 2046K /* 2 MB − 2K ISR */
    SRAM1 (rwx): ORIGIN = 0x20060000, LENGTH = 384K  /* Superblocks 24-47 of SRAM1 (non-secure) */
}

PROVIDE(_estack = ORIGIN(SRAM1) + LENGTH(SRAM1) - 4);

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
    } > SRAM1 AT> FLASH

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
    } > SRAM1

    . = ALIGN(8);
    _end = .;
    PROVIDE(end = .);
}
