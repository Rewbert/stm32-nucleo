#ifndef BACKENDS_STM32L5_TZSC_H
#define BACKENDS_STM32L5_TZSC_H

#include "drivers/tzsc.h"
#include "stm32l5xx.h"

typedef struct {
    GTZC_TZSC_TypeDef *tzsc;
} stm32l5_tzsc_backend_t;

void stm32l5_tzsc_create(tzsc_dev_t *dev, stm32l5_tzsc_backend_t *backend);

#endif // BACKENDS_STM32L5_TZSC_H
