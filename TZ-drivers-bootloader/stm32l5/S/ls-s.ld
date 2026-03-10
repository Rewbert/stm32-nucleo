ENTRY(reset_handler)

MEMORY
{
    ISR       (rw):  ORIGIN = 0x0C000000, LENGTH = 2K   /* ISR vector */
    FLASH     (rx):  ORIGIN = 0x0C000800, LENGTH = 248K /* Second of half FLASH, minus 4K for NSC, and minus 2K for PERSIST */
    PERSIST   (rw):  ORIGIN = 0x0C03E800, LENGTH = 2K   /* Segment for persistent, mutable, data */
    FLASH_NSC (rx):  ORIGIN = 0x0C03F000, LENGTH = 4K   /* NSC region at end of FLASH */
    SRAM1     (rwx): ORIGIN = 0x30000000, LENGTH = 128K  /* first of half SRAM1 */
}

PROVIDE(_estack = ORIGIN(SRAM1) + LENGTH (SRAM1) - 4);

SECTIONS
{
    .isr_vector :
    {
        KEEP(*(.isr_vector)) /* The code does not refer to isr_vector, so we must ask to keep it */
    } > ISR

    .gnu.sgstubs :
    {
        . = ALIGN(8);
        *(.gnu.sgstubs*)
    } > FLASH_NSC

    .nsc_veneers :
    {
        . = ALIGN(8);
        KEEP(*(.nsc_veneers)) /* These are related to the TrustZone veneers */
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