/*
 * The stm32l5xx.h include below is the only STM32-specific dependency; any other device header that pulls in core_cm33.h will do.
 */

#include <stdint.h>
#include "stm32l5xx.h"           /* for SysTick_Config, TZ_SysTick_Config_NS */
#include "domain/cmsis_select.h" /* for HAL_SECURE */
#include "drivers/systick.h"

static volatile uint32_t ticks;

void systick_configure(uint32_t reload) {
    SysTick_Config(reload);
#if HAL_SECURE
    TZ_SysTick_Config_NS(reload);
#endif
}

uint32_t systick_get_ticks(void) {
    return ticks;
}

__attribute__((weak)) void systick_callback(uint32_t t) {
    (void)t;
}

void systick_handler(void) {
    ticks++;
    systick_callback(ticks);
}

void systick_delay_ms(uint32_t ms) {
    uint32_t start = ticks;
    uint32_t end   = start + ms;

    if (end < start) {        /* overflow: wait for ticks to wrap past start first */
        while (ticks > start);
    }
    while (ticks < end);
}
