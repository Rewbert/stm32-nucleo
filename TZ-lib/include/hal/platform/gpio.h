#ifndef PLATFORM_GPIO_MAP_H
#define PLATFORM_GPIO_MAP_H

#include "hal/core/gpio.h"
#include "hal/platform/domain.h"
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

// /**
//  * @brief Set GPIO alternate function (platform-specific)
//  */
// static inline void platform_gpio_set_af(gpio_t gpio, gpio_af_t af) {
// #if HAL_SECURE
//     GPIO_TypeDef *port = gpio_port_base(gpio.port);
    
//     uint32_t index = gpio.pin >> 3;        // 0 for pins 0–7, 1 for 8–15
//     uint32_t shift = (gpio.pin & 0x7U) * 4U;
//     uint32_t mask  = 0xFU << shift;
    
//     port->AFR[index] &= ~mask;
//     port->AFR[index] |= ((uint32_t)af << shift);
// #endif
// }

#define GPIO_AF_MASK 0xFU

/**
 * @brief Set the alternating function of a GPIO pin.
 * 
 */
static inline void platform_gpio_set_af(gpio_t gpio, gpio_af_t af) {
#if HAL_SECURE
    GPIO_TypeDef *port = gpio_port_base(gpio.port);

    if(gpio.pin >= 0 && gpio.pin < 8) {
        port->AFR[0] &= ~(GPIO_AF_MASK << (gpio.pin * 4));
        port->AFR[0] |= (af << (gpio.pin * 4));
    } else if (gpio.pin >= 8 && gpio.pin < 15) {
        port->AFR[1] &= ~(GPIO_AF_MASK << ((gpio.pin - 8) * 4));
        port->AFR[1] |= (af << ((gpio.pin - 8) * 4));
    }
#endif
}

#endif // PLATFORM_GPIO_MAP_H
