#ifndef BACKENDS_STM32U5_TIMER_H
#define BACKENDS_STM32U5_TIMER_H

#include "drivers/timer.h"
#include "stm32u5xx.h"

typedef struct {
    TIM_TypeDef *tim;
} stm32u5_timer_backend_t;

void stm32u5_timer_create(timer_dev_t *dev, TIM_TypeDef *tim, stm32u5_timer_backend_t *backend);

#endif // BACKENDS_STM32U5_TIMER_H
