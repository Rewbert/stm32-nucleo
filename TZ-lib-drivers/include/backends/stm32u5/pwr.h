#ifndef BACKENDS_STM32U5_PWR_H
#define BACKENDS_STM32U5_PWR_H

#include "drivers/pwr.h"

/* No backend storage required — this driver accesses PWR registers directly via PWRx macro. */
void stm32u5_pwr_create(pwr_dev_t *dev);

#endif // BACKENDS_STM32U5_PWR_H
