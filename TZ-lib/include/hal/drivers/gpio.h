#ifndef DRIVER_GPIO_H
#define DRIVER_GPIO_H

#include "hal/core/core_gpio.h"

void toggle_gpio(gpio_t gpio);
void set_gpio(gpio_t gpio, gpio_level_t level);
gpio_level_t get_gpio(gpio_t gpio); 

#endif // DRIVER_GPIO_H