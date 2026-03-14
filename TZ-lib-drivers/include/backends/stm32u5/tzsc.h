#ifndef BACKENDS_STM32U5_TZSC_H
#define BACKENDS_STM32U5_TZSC_H

#include "drivers/tzsc.h"
#include "stm32u5xx.h"

/* U5 has two GTZC instances: GTZC1 covers main bus peripherals (APB1/APB2/AHB2),
 * GTZC2 covers low-power bus peripherals (APB3/AHB3), including LPUART1. */
typedef struct {
    GTZC_TZSC_TypeDef *tzsc1; /* GTZC_TZSC1 */
    GTZC_TZSC_TypeDef *tzsc2; /* GTZC_TZSC2 */
} stm32u5_tzsc_backend_t;

void stm32u5_tzsc_create(tzsc_dev_t *dev, stm32u5_tzsc_backend_t *backend);

#endif // BACKENDS_STM32U5_TZSC_H
