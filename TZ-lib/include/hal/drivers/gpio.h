#ifndef DRIVER_GPIO_H
#define DRIVER_GPIO_H

#include "hal/core/core_gpio.h"

void toggle_gpio(gpio_t gpio);
void set_gpio(gpio_t gpio, gpio_level_t level);
gpio_level_t get_gpio(gpio_t gpio); 

void gpio_set_pupdr(gpio_t gpio, gpio_pupdr_t pupdr);
void gpio_set_mode(gpio_t gpio, gpio_mode_t mode)

#endif // DRIVER_GPIO_H