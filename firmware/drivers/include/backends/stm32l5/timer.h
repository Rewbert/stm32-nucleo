#ifndef BACKENDS_STM32L5_TIMER_H
#define BACKENDS_STM32L5_TIMER_H

#include "drivers/timer.h"
#include "stm32l5xx.h"

typedef struct {
    TIM_TypeDef *tim;
} stm32l5_timer_backend_t;

void stm32l5_timer_create(timer_dev_t *dev, TIM_TypeDef *tim, stm32l5_timer_backend_t *backend);

#endif // BACKENDS_STM32L5_TIMER_H
