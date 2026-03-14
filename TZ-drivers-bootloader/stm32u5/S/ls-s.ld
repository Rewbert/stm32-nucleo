ENTRY(reset_handler)

/* STM32U5A5ZJQ — 4 MB flash, 2.5 MB SRAM total.
 * Secure region: first 2 MB of flash (0x0C000000 secure-bus alias).
 * Stack: top of the 384 KB secure SRAM1 region (superblocks 0-23). */

MEMORY
{
    ISR       (rw):  ORIGIN = 0x0C000000, LENGTH = 2K    /* ISR vector table (512 words) */
    FLASH     (rx):  ORIGIN = 0x0C000800, LENGTH = 2040K /* 2 MB − 2K ISR − 2K PERSIST − 4K NSC */
    PERSIST   (rw):  ORIGIN = 0x0C1FE800, LENGTH = 2K    /* Persistent mutable data in flash */
    FLASH_NSC (rx):  ORIGIN = 0x0C1FF000, LENGTH = 4K    /* Non-secure callable veneers */
    SRAM1     (rwx): ORIGIN = 0x30000000, LENGTH = 384K  /* Superblocks 0-23 of SRAM1 (secure) */
}

PROVIDE(_estack = ORIGIN(SRAM1) + LENGTH(SRAM1) - 4);

SECTIONS
{
    .isr_vector :
    {
        KEEP(*(.isr_vector))
    } > ISR

    .gnu.sgstubs :
    {
        . = ALIGN(8);
        *(.gnu.sgstubs*)
    } > FLASH_NSC

    .nsc_veneers :
    {
        . = ALIGN(8);
        KEEP(*(.nsc_veneers))
    } > FLASH_NSC

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

    .persist (NOLOAD) :
    {
        . = ALIGN(8);
        __persist_start__ = .;
        KEEP(*(.persist))
        KEEP(*(.persist.*))
        . = ALIGN(8);
        __persist_end__ = .;
    } > PERSIST
}
