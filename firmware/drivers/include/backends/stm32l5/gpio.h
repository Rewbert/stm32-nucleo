#ifndef BACKENDS_STM32L5_GPIO_H
#define BACKENDS_STM32L5_GPIO_H

#include <stdint.h>
#include "drivers/gpio.h"
#include "stm32l5xx.h"

typedef struct {
    GPIO_TypeDef *gpio;
    uint8_t       pin;
} stm32l5_gpio_backend_t;

void stm32l5_gpio_create(gpio_dev_t *dev,
                         GPIO_TypeDef *port,
                         uint8_t pin,
                         stm32l5_gpio_backend_t *backend);

#endif // BACKENDS_STM32L5_GPIO_H
