
#include "hal/platform/cmsis_select.h"
#include "hal/platform/gpio.h"
#include "hal/core/gpio.h"
#include "hal/drivers/gpio.h"

/**
 * @brief Toggle the value of a pin, flipping it from high to low, or low to high.
 * 
 * @param gpio HAL level GPIO object
 */
void toggle_gpio(gpio_t gpio) {
    GPIO_TypeDef *port = gpio_port_base(gpio.port);
    int mask = gpio_pin_mask(gpio);
    port->ODR ^= mask;
}

/**
 * @brief Set the value gpio object
 * 
 * @param gpio HAL level GPIO object
 * @param level New level of the pin
 */
void set_gpio(gpio_t gpio, gpio_level_t level) {
    GPIO_TypeDef *port = gpio_port_base(gpio.port);
    int mask = gpio_pin_mask(gpio);
    if(level == LOW) {
        port->ODR &= ~mask;
    } else {
        port->ODR |= mask;
    }
}

/**
 * @brief Get the level of the gpio
 * 
 * @param gpio HAL level GPIO object
 * @return gpio_level_t current level of the pin
 */
gpio_level_t get_gpio(gpio_t gpio) {
    GPIO_TypeDef *port = gpio_port_base(gpio.port);
    int v = (port->IDR >> gpio.pin) & 1;
    return v ? HIGH : LOW;
}