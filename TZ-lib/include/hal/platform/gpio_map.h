#ifndef PLATFORM_GPIO_MAP_H
#define PLATFORM_GPIO_MAP_H

#include "hal/core/gpio.h"
#include "hal/platform/cmsis_select.h"
#include "stm32l5xx.h"

/**
 * @brief Convert HAL gpio port to CMSIS GPIO register block
 */
static inline GPIO_TypeDef *gpio_port_base(gpio_port_t port) {
    switch (port) {
        case GPIO_PORT_A: return GPIO_CMSIS(A);
        case GPIO_PORT_B: return GPIO_CMSIS(B);
        case GPIO_PORT_C: return GPIO_CMSIS(C);
        case GPIO_PORT_D: return GPIO_CMSIS(D);
        case GPIO_PORT_E: return GPIO_CMSIS(E);
        case GPIO_PORT_F: return GPIO_CMSIS(F);
        case GPIO_PORT_G: return GPIO_CMSIS(G);
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
 * @brief Convert HAL gpio pin into corresponding CMSIS 2bit shift
 */
static inline uint32_t gpio_2bit_shift(uint8_t pin) {
    return pin * 2U;
}

/**
 * @brief Convert HAL gpio pin into corresponding CMSIS 2bit mask
 */
static inline uint32_t gpio_2bit_mask(uint8_t pin) {
    return 0x3U << gpio_2bit_shift(pin);
}

#endif // PLATFORM_GPIO_MAP_H
