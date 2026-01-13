#ifndef PLATFORM_CLOCK_H
#define PLATFORM_CLOCK_H

#include "stm32l5xx.h"

/**
 * @brief Enable the peripheral clock for a GPIO port.
 *
 * On STM32L552, GPIO_PORT_G is special and requires enabling the IOSV power
 * domain via PWR before the GPIO clock can be used.
 */
static inline void platform_clock_enable_gpio(gpio_port_t port) {
#if HAL_SECURE
    if(port == GPIO_PORT_G) {
        RCCx->APB1ENR1 |= RCC_APB1ENR1_PWREN;
        PWRx->CR2 |= PWR_CR2_IOSV;
    }
    
    volatile uint32_t dummy;
    RCCx->AHB2ENR |= (1U << port);
    dummy = RCCx->AHB2ENR; // need to do some reads to let enough cycles elapse such that the power is on
    dummy = RCCx->AHB2ENR;
#endif
}

#endif // PLATFORM_CLOCK_H