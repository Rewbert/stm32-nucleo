#include <stdint.h>
#define VECTOR_SIZE_WORDS 464 // somehow I concluded that we need 464 here... but now I cannot remember how

extern uint32_t _estack;

#include "stm32l5xx.h"

void default_handler(void);
void reset_handler(void);
void HardFault_Handler(void);
void secure_fault(void);

// override these later as needed
void nmi_handler(void) __attribute__((weak, alias("default_handler")));
void mem_handler(void) __attribute__((weak, alias("default_handler")));
void bus_handler(void) __attribute__((weak, alias("default_handler")));
void usage_handler(void) __attribute__((weak, alias("default_handler")));
void sv_handler(void) __attribute__((weak, alias("default_handler")));
void debug_handler(void) __attribute__((weak, alias("default_handler")));
void pend_handler(void) __attribute__((weak, alias("default_handler")));
void systick_handler(void) __attribute__((weak, alias("default_handler")));

// goes in special section, it has to end up there for everything to work. Look in the reference manual, the part about vector table
uint32_t isr_vector[VECTOR_SIZE_WORDS] __attribute__((section(".isr_vector"))) = {
    (uint32_t) &_estack, // The first entry is the initial stack pointer
    (uint32_t)&reset_handler,
    (uint32_t)&nmi_handler,
    (uint32_t)&HardFault_Handler,
    (uint32_t)&mem_handler,
    (uint32_t)&bus_handler,
    (uint32_t)&usage_handler,
    (uint32_t)&secure_fault,
    0,
    0,
    0,
    (uint32_t)&sv_handler,
    (uint32_t)&debug_handler,
    0,
    (uint32_t)&pend_handler,
    (uint32_t)&systick_handler,
    // add more handlers as wanted, I believe we can also add IRQs here, which we can use for fun features
};

// these two are identical, but we can distinguish which one we are in with GDB
void default_handler(void) {
    while(1);
}

void HardFault_Handler(void) {
  while(1);
}

void secure_fault(void) {
  while(1);
}

extern uint32_t _etext, _sdata, _sidata, _edata, _sbss, _ebss; // symbols defined by the linker

// void main(void);
// extern void __libc_init_array();

void main(void) {
  while(1) {}
}

void reset_handler(void) {
    // Copy .data from FLASH to SRAM
    uint32_t data_size = (uint32_t)&_edata - (uint32_t)&_sdata;
    uint8_t *flash_data = (uint8_t*) &_sidata;
    uint8_t *sram_data = (uint8_t*) &_sdata;
  
    for (uint32_t i = 0; i < data_size; i++)
    {
      sram_data[i] = flash_data[i];
    }

    // Zero-fill .bss section in SRAM
    uint32_t bss_size = (uint32_t)&_ebss - (uint32_t)&_sbss;
    uint8_t *bss = (uint8_t*) &_sbss;

    for (uint32_t i = 0; i < bss_size; i++)
    {
      bss[i] = 0;
    }

    // __libc_init_array(); // this is from newlib nano. Appears to be a no op right now, but might change depending on what we add
    main();
}