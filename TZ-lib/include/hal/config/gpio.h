#ifndef CONFIG_GPIO_H
#define CONFIG_GPIO_H

#include "hal/core/core_gpio.h"

void gpio_make_secure(gpio_t gpio);
void gpio_make_nonsecure(gpio_t gpio);

#endif // CONFIG_GPIO_H