
#include "hal/platform/cmsis_select.h"
#include "hal/platform/gpio_map.h"
#include "hal/core/core_gpio.h"
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
    int v = (port->ODR >> gpio.pin) & 1;
    return v ? HIGH : LOW;
}

/**
 * @brief Set the pull-up pull-down state of a pin
 * 
 * @param gpio HAL level GPIO object
 * @param pupdr desired pull-up pull-down state
 */
void gpio_set_pupdr(gpio_t gpio, gpio_pupdr_t pupdr) {
    GPIO_TypeDef *port = gpio_port_base(gpio.port);
    uint32_t shift = gpio_2bit_shift(gpio.pin);
    uint32_t mask = gpio_2bit_mask(gpio.pin);

    port->PUPDR &= ~mask;
    port->PUPDR |= ((uint32_t)pupdr << shift);
}

/**
 * @brief Set GPIO pin mode
 *
 * @param gpio HAL level GPIO object
 * @param mode Desired GPIO mode
 */
 void gpio_set_mode(gpio_t gpio, gpio_mode_t mode) {
    GPIO_TypeDef *port = gpio_port_base(gpio.port);
    uint32_t shift = gpio_moder_shift(gpio.pin);
    uint32_t mask  = gpio_moder_mask(gpio.pin);

    port->MODER &= ~mask;
    port->MODER |= ((uint32_t)mode << shift);
}