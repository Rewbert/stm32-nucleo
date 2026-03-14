#ifndef BACKENDS_STM32U5_EXTI_H
#define BACKENDS_STM32U5_EXTI_H

#include <stdint.h>
#include "drivers/exti.h"

typedef struct {
    uint8_t pin;
} stm32u5_exti_backend_t;

void stm32u5_exti_create(exti_dev_t *dev, stm32u5_exti_backend_t *backend, uint8_t pin);

#endif // BACKENDS_STM32U5_EXTI_H
