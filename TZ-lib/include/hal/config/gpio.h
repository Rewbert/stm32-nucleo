#ifndef CONFIG_GPIO_H
#define CONFIG_GPIO_H

#include "hal/core/gpio.h"

void gpio_make_secure(gpio_t gpio);
void gpio_make_nonsecure(gpio_t gpio);

void gpio_set_pupdr(gpio_t gpio, gpio_pupdr_t pupdr);
void gpio_set_mode(gpio_t gpio, gpio_mode_t mode);

#endif // CONFIG_GPIO_H