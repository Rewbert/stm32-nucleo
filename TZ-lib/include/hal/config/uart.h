#ifndef CONFIG_UART_H
#define CONFIG_UART_H

#include <stdint.h>
#include "hal/core/uart.h"
#include "hal/core/clock.h"

/**
 * @brief Select the clock source for the specified UART.
 * 
 */
void uart_select_clock_source(uart_t uart, clock_source_t clock);

/**
 * @brief Enable the power to a specific UART.
 * 
 */
void uart_enable_power(uart_t uart);

/**
 * @brief Activate a specific UART.
 * 
 */
void uart_activate(uart_t uart);

/**
 * @brief Set the BRR register value of a specific UART.
 * 
 */
void uart_set_brr(uart_t uart, uint32_t brr);

#endif // CONFIG_UART_H