#include <stdint.h>

/* STM32U5 has IRQ numbers up to 131 (HSPI1_IRQn). The vector table requires
 * 1 (initial SP) + 15 (core exceptions) + 132 (external IRQs) = 148 words.
 * We allocate 512 words (= 2K) to match the ISR region in the linker script. */
#define VECTOR_SIZE_WORDS 512

#include "stm32u5xx.h"

extern uint32_t _estack;

void default_handler(void);
void reset_handler(void);
void HardFault_Handler(void);

void nmi_handler(void)      __attribute__((weak, alias("default_handler")));
void mem_handler(void)      __attribute__((weak, alias("default_handler")));
void bus_handler(void)      __attribute__((weak, alias("default_handler")));
void usage_handler(void)    __attribute__((weak, alias("default_handler")));
void sv_handler(void)       __attribute__((weak, alias("default_handler")));
void debug_handler(void)    __attribute__((weak, alias("default_handler")));
void pend_handler(void)     __attribute__((weak, alias("default_handler")));
void systick_handler(void)  __attribute__((weak, alias("default_handler")));

void exti_default_handler(void);

void exti0_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti1_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti2_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti3_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti4_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti5_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti6_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti7_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti8_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti9_handler(void)  __attribute__((weak, alias("exti_default_handler")));
void exti10_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti11_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti12_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti13_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti14_handler(void) __attribute__((weak, alias("exti_default_handler")));
void exti15_handler(void) __attribute__((weak, alias("exti_default_handler")));

void exti_default_handler(void) {
    while(1) {}
}

uint32_t isr_vector[VECTOR_SIZE_WORDS] __attribute__((section(".isr_vector"))) = {
    (uint32_t)&_estack,
    (uint32_t)&reset_handler,
    (uint32_t)&nmi_handler,
    (uint32_t)&HardFault_Handler,
    (uint32_t)&mem_handler,
    (uint32_t)&bus_handler,
    (uint32_t)&usage_handler,
    0, /* SecureFault — not visible from NS world */
    0,
    0,
    0,
    (uint32_t)&sv_handler,
    (uint32_t)&debug_handler,
    0,
    (uint32_t)&pend_handler,
    (uint32_t)&systick_handler,
    /* External IRQs (offset 16+) — only EXTI0-15 wired, rest are 0 */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* IRQ0-10 */
    (uint32_t)&exti0_handler,  /* IRQ11 = EXTI0 */
    (uint32_t)&exti1_handler,
    (uint32_t)&exti2_handler,
    (uint32_t)&exti3_handler,
    (uint32_t)&exti4_handler,
    (uint32_t)&exti5_handler,
    (uint32_t)&exti6_handler,
    (uint32_t)&exti7_handler,
    (uint32_t)&exti8_handler,
    (uint32_t)&exti9_handler,
    (uint32_t)&exti10_handler,
    (uint32_t)&exti11_handler,
    (uint32_t)&exti12_handler,
    (uint32_t)&exti13_handler,
    (uint32_t)&exti14_handler,
    (uint32_t)&exti15_handler,
};

void default_handler(void) {
    while(1);
}

void HardFault_Handler(void) {
    while(1);
}

extern uint32_t _etext, _sdata, _sidata, _edata, _sbss, _ebss;

void main(void);

void reset_handler(void) {
    uint32_t data_size = (uint32_t)&_edata - (uint32_t)&_sdata;
    uint8_t *flash_data = (uint8_t*)&_sidata;
    uint8_t *sram_data  = (uint8_t*)&_sdata;

    for (uint32_t i = 0; i < data_size; i++) {
        sram_data[i] = flash_data[i];
    }

    uint32_t bss_size = (uint32_t)&_ebss - (uint32_t)&_sbss;
    uint8_t *bss = (uint8_t*)&_sbss;

    for (uint32_t i = 0; i < bss_size; i++) {
        bss[i] = 0;
    }

    main();
}
