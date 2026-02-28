#ifndef BACKENDS_STM32L5_RCC_H
#define BACKENDS_STM32L5_RCC_H

#include "drivers/rcc.h"

/* No backend storage required — this driver accesses RCC registers directly via EXTIx macros. */
void stm32l5_rcc_create(rcc_dev_t *dev);

#endif // BACKENDS_STM32L5_RCC_H
