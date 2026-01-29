
#include "hal/core/uart.h"
#include "hal/core/clock.h"
#include "hal/config/uart.h"
#include "hal/config/clock.h"
#include "hal/platform/uart.h"

/**
 * @brief Select the clock source that will be used by the UART peripheral. The clock source, and
 * its frequency, will be used to determine e.g. BRR register values, etc.
 * 
 */
void uart_select_clock_source(uart_t uart, clock_source_t clock) {
    switch(uart) {
        case HAL_USART1: break; // only LPUART1 implemented
        case HAL_USART2: break;
        case HAL_USART3: break;
        case HAL_UART4: break;
        case HAL_UART5: break;
        case HAL_LPUART1: platform_lpuart1_select_clock_source(clock); break;
        default: break;
    }
}

/**
 * @brief Enable the clock to the UART peripheral.
 * 
 */
void uart_enable_power(uart_t uart) {
    switch(uart) {
        case HAL_USART1: break; // only LPUART1 implemented
        case HAL_USART2: break;
        case HAL_USART3: break;
        case HAL_UART4: break;
        case HAL_UART5: break;
        case HAL_LPUART1: clock_enable_uart(uart); break;
        default: break;
    }
}

/**
 * @brief Activate the UART peripheral.
 * 
 */
void uart_activate(uart_t uart) {
    switch(uart) {
        case HAL_USART1: break; // only LPUART1 implemented
        case HAL_USART2: break;
        case HAL_USART3: break;
        case HAL_UART4: break;
        case HAL_UART5: break;
        case HAL_LPUART1: platform_lpuart1_activate(); break;
        default: break;
    }
}

/**
 * @brief Set the BRR register value of the UART peripheral.
 * 
 */
void uart_set_brr(uart_t uart, uint32_t brr) {
    switch(uart) {
        case HAL_USART1: break; // only LPUART1 implemented
        case HAL_USART2: break;
        case HAL_USART3: break;
        case HAL_UART4: break;
        case HAL_UART5: break;
        case HAL_LPUART1: platform_set_lpuart1_brr(brr); break;
        default: break;
    }
}