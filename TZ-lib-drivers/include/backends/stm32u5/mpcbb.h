#ifndef BACKENDS_STM32U5_MPCBB_H
#define BACKENDS_STM32U5_MPCBB_H

#include "drivers/mpcbb.h"
#include "stm32u5xx.h"

typedef struct {
    GTZC_MPCBB_TypeDef *mpcbb;
} stm32u5_mpcbb_backend_t;

void stm32u5_mpcbb_create(mpcbb_dev_t *dev,
                           GTZC_MPCBB_TypeDef *mpcbb,
                           stm32u5_mpcbb_backend_t *backend);

#endif // BACKENDS_STM32U5_MPCBB_H
