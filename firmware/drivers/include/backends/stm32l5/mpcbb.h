#ifndef BACKENDS_STM32L5_MPCBB_H
#define BACKENDS_STM32L5_MPCBB_H

#include "drivers/mpcbb.h"
#include "stm32l5xx.h"

typedef struct {
    GTZC_MPCBB_TypeDef *mpcbb;
} stm32l5_mpcbb_backend_t;

void stm32l5_mpcbb_create(mpcbb_dev_t *dev, GTZC_MPCBB_TypeDef *mpcbb, stm32l5_mpcbb_backend_t *backend);

#endif // BACKENDS_STM32L5_MPCBB_H