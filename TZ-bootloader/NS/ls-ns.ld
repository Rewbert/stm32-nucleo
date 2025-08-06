ENTRY(reset_handler)

MEMORY
{
    ISR   (rw):  ORIGIN = 0x08040000, LENGTH = 2K /* ISR vector */
    FLASH (rx):  ORIGIN = 0x08040800, LENGTH = 254K /* First half of FLASH */
    SRAM1 (rwx): ORIGIN = 0x20018000, LENGTH = 96K  /* First half of SRAM1 */
}

PROVIDE(_estack = ORIGIN(SRAM1) + LENGTH (SRAM1) - 4);

SECTIONS
{
    .isr_vector :
    {
        KEEP(*(.isr_vector)) /* The code does not refer to isr_vector, so we must ask to keep it */
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