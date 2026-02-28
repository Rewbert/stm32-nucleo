#ifndef BACKENDS_STM32L5_EXTI_H
#define BACKENDS_STM32L5_EXTI_H

#include <stdint.h>
#include "drivers/exti.h"

typedef struct {
    uint8_t pin;
} stm32l5_exti_backend_t;

void stm32l5_exti_create(exti_dev_t *dev, stm32l5_exti_backend_t *backend);

#endif // BACKENDS_STM32L5_EXTI_H
