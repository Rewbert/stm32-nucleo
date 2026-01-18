#ifndef SERVICES_BUTTON_H
#define SERVICES_BUTTON_H

#include "hal/platform/domain.h"
#include "hal/core/gpio.h"

void init_button(gpio_t gpio, exti_edge_t edge, security_domain_t domain);

#endif // SERVICES_BUTTON_H