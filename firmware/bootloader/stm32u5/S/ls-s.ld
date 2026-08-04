ENTRY(reset_handler)

/* STM32U5A5ZJQ — 4 MB flash, 2.5 MB SRAM total.
 * Secure flash: first 2 MB (0x0C000000 secure-bus alias).
 * Secure SRAM: 1248 KB contiguous block — SRAM1 (768K) + SRAM2 (64K) + SRAM3 first 416K (SBs 0-25).
 * Stack: top of SRAM at 0x30138000. */

MEMORY
{
    ISR       (rw):  ORIGIN = 0x0C000000, LENGTH = 2K    /* ISR vector table (512 words) */
    FLASH     (rx):  ORIGIN = 0x0C000800, LENGTH = 2022K /* 2 MB − 2K ISR − 16K PERSIST − 8K NSC */
    PERSIST   (rw):  ORIGIN = 0x0C1FA000, LENGTH = 16K   /* two 8 KB pages of persistent flash storage (udb) */
    FLASH_NSC (rx):  ORIGIN = 0x0C1FE000, LENGTH = 8K    /* Non-secure callable veneers (one full 8 KB page) */
    SRAM      (rwx): ORIGIN = 0x30000000, LENGTH = 1248K /* SRAM1+SRAM2+SRAM3[SB 0-25] */
}

PROVIDE(_estack = ORIGIN(SRAM) + LENGTH(SRAM) - 4);

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

    /* PERSIST is the two 8 KB pages the udb database lives in (segments at
     * 0x0C1FA000 and 0x0C1FC000). It is NOLOAD, so the linker never emits image
     * bytes here and a flash-and-reset leaves the database intact. The udb
     * driver addresses it directly; keep examples/udb-test/shared/stm32u5-drv.c
     * (UDB_FLASH_END = 0x0C1FE000) in sync with this region. */
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
