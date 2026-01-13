#ifndef CONFIG_CLOCK_H
#define CONFIG_CLOCK_H

#include "hal/core/gpio.h"
#include "hal/core/clock.h"

void clock_enable_gpio(gpio_port_t port);

#endif // CONFIG_CLOCK_H