
#include "hal/platform/domain.h"
#include "hal/platform/cmsis_select.h"
#include "hal/core/gpio.h"
#include "hal/core/clock.h"
#include "hal/config/clock.h"
#include "hal/platform/clock.h"

/**
 * @brief Enable the clock for a specific GPIO port.
 */
void clock_enable_gpio(gpio_port_t port) {
    platform_clock_enable_gpio(port);
}

/**
 * @brief Enable the clock for a specific UART. The only implemented UART right now is the
 * low power UART (LPUART1) peripheral, but the board has support for 4 other ones.
 * 
 */
void clock_enable_uart(uart_t uart) {
    switch(uart) {
        case HAL_USART1: break; // only LPUART1 implemented
        case HAL_USART2: break;
        case HAL_USART3: break;
        case HAL_UART4: break;
        case HAL_UART5: break;
        case HAL_LPUART1: platform_clock_enable_lpuart1(); break;
        default: break;
    }
}