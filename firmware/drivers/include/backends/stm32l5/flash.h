#ifndef BACKENDS_STM32L5_FLASH_H
#define BACKENDS_STM32L5_FLASH_H

#include "drivers/flash.h"
#include "stm32l5xx.h"

typedef struct {
    FLASH_TypeDef *flash;
} stm32l5_flash_backend_t;

void stm32l5_flash_create(flash_dev_t *dev,
                           FLASH_TypeDef *flash,
                           stm32l5_flash_backend_t *backend);

#endif // BACKENDS_STM32L5_FLASH_H
