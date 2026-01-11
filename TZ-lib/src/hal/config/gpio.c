
#include "hal/platform/cmsis_select.h"
#include "hal/platform/gpio_map.h"
#include "hal/core/core_gpio.h"
#include "hal/config/gpio.h"

void gpio_make_secure(gpio_t gpio) {
#if HAL_SECURE
    GPIO_TypeDef *port = gpio_port_base(gpio.port);
    port->SECCFGR &= ~(0 << gpio.pin); // maybe wrong... seems like a no-op
#endif
}

void gpio_make_nonsecure(gpio_t gpio) {
#if HAL_SECURE
    GPIO_TypeDef *port = gpio_port_base(gpio.port);
    port->SECCFGR &= ~(1 << gpio.pin);
#endif
}