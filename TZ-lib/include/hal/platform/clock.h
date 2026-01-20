#ifndef PLATFORM_CLOCK_H
#define PLATFORM_CLOCK_H

#include "hal/core/gpio.h"
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

/**
 * @brief Configure the MCU on the STM32L552ZEQ to run at 110 MHz.
 *
 */
void platform_clock_configure_110mhz(void);

/**
 * @brief Configure how often the systick interrupt is invoked
 * 
 */
void configure_systick(int tick);

/**
 * @brief Enable the clock for the LPUART1.
 * 
 */
static inline void platform_clock_enable_lpuart1() {
#if HAL_SECURE
    volatile uint32_t dummy;
    RCCx->APB1ENR2 |= (0x1U << RCC_APB1ENR2_LPUART1EN_Pos);
    dummy = RCCx->APB1ENR2;
    dummy = RCCx->APB1ENR2;
#endif
}

/**
 * @brief Sysclock will increment this. Can be observed to measure passage of time.
 * 
 */
extern volatile uint32_t ticks;

/**
 * @brief Systick timer. Invoked by the CPU at an interval decided by `configure_systick`.
 * 
 */
void systick_handler();

#endif // PLATFORM_CLOCK_H