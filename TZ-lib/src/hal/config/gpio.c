
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

/**
 * @brief Set the pull-up pull-down state of a pin
 * 
 * @param gpio HAL level GPIO object
 * @param pupdr desired pull-up pull-down state
 */
void gpio_set_pupdr(gpio_t gpio, gpio_pupdr_t pupdr) {
#if HAL_SECURE
    GPIO_TypeDef *port = gpio_port_base(gpio.port);
    uint32_t shift = gpio_2bit_shift(gpio.pin);
    uint32_t mask = gpio_2bit_mask(gpio.pin);

    port->PUPDR &= ~mask;
    port->PUPDR |= ((uint32_t)pupdr << shift);
#endif
}

/**
 * @brief Set GPIO pin mode
 *
 * @param gpio HAL level GPIO object
 * @param mode Desired GPIO mode
 */
void gpio_set_mode(gpio_t gpio, gpio_mode_t mode) {
#if HAL_SECURE
    GPIO_TypeDef *port = gpio_port_base(gpio.port);
    uint32_t shift = gpio_2bit_shift(gpio.pin);
    uint32_t mask  = gpio_2bit_mask(gpio.pin);

    port->MODER &= ~mask;
    port->MODER |= ((uint32_t)mode << shift);
#endif
}