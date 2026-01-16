#ifndef CONFIG_CLOCK_H
#define CONFIG_CLOCK_H

#include "hal/core/gpio.h"
#include "hal/core/uart.h"
#include "hal/core/clock.h"

/**
 * @brief Enable the GPIO clock associated with a particular GPIO port.
 * 
 */
void clock_enable_gpio(gpio_port_t port);

/**
 * @brief Enable the clock associated with a particular UART.
 * 
 */
void clock_enable_uart(uart_t uart);

#endif // CONFIG_CLOCK_H