ENTRY(reset_handler)

MEMORY
{
    ISR   (rw):  ORIGIN = 0x08000000, LENGTH = 2K
    FLASH (rx):  ORIGIN = 0x08000800, LENGTH = 510K
    SRAM1 (rwx): ORIGIN = 0x20000000, LENGTH = 192K
}

SECTIONS
{
    .isr_vector :
    {
        KEEP(*(.isr_vector))
    } >ISR

    .text :
    {
        . = ALIGN(4);
        *(.text)
        *(.text.*)
        *(.rodata)
        *(.rodata.*)

        __exidx_start = .;
        KEEP(*(.ARM.exidx*))
        __exidx_end = .;

        . = ALIGN(4);
        _etext = .;
    } >FLASH

    .data :
    {
        . = ALIGN(4);
        _sdata = .;
        *(.data)
        *(.data.*)
        . = ALIGN(4);
        _edata = .;
    } >SRAM1 AT> FLASH

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
    } >SRAM1

    . = ALIGN(8);
    _end = .;
    PROVIDE(end = .);
}
