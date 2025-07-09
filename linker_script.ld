ENTRY(reset_handler)

MEMORY
{
    FLASH (rx):  ORIGIN = 0x08000000, LENGTH = 512K
    SRAM1 (rwx): ORIGIN = 0x20000000, LENGTH = 192K
}

SECTIONS
{
    .isr_vector :
    {
        KEEP(*(.isr_vector))
    } >FLASH

    .text :
    {
        . = ALIGN(4);
        *(.text)
        *(.text.*)
        *(.rodata)
        *(.rodata.*)
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