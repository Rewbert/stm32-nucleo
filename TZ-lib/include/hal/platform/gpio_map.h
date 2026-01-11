#ifndef PLATFORM_GPIO_MAP_H
#define PLATFORM_GPIO_MAP_H

#include "hal/core/core_gpio.h"
#include "hal/platform/cmsis_select.h"
#include "stm32l5xx.h"

/**
 * @brief Convert HAL gpio port to CMSIS GPIO register block
 */
static inline GPIO_TypeDef *gpio_port_base(gpio_port_t port) {
    switch (port) {
        case GPIO_PORT_A: return GPIO(A);
        case GPIO_PORT_B: return GPIO(B);
        case GPIO_PORT_C: return GPIO(C);
        case GPIO_PORT_D: return GPIO(D);
        case GPIO_PORT_E: return GPIO(E);
        case GPIO_PORT_F: return GPIO(F);
        case GPIO_PORT_G: return GPIO(G);
        default:          return 0;
    }
}

/**
 * @brief Return pin mask (1 << pin)
 */
static inline uint32_t gpio_pin_mask(gpio_t gpio) {
    return (1U << gpio.pin);
}

/**
 * @brief Convert HAL gpio pin into corresponding CMSIS PUPDR shift
 */
 static inline uint32_t gpio_pupdr_shift(uint8_t pin) {
    // The PUPDR register reserves 2 bits per pin
    return (pin * 2U);
}

/**
 * @brief Convert HAL gpio pin into corresponding CMSIS PUPDR mask
 */
static inline uint32_t gpio_pupdr_mask(uint8_t pin) {
    // The PUPDR register reserves 2 bits per pin, which 0x3U represents
    uint32_t shift = gpio_pupdr_shift(pin);
    return 0x3U << shift;
}

#endif // PLATFORM_GPIO_MAP_H
