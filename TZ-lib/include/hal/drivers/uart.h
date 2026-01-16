#ifndef DRIVER_UART_H
#define DRIVER_UART_H

/**
 * @brief Write a character to the specified UART.
 * 
 */
void uart_write(uart_t uart, char c);

/**
 * @brief Read a character from the specified UART.
 * 
 */
char uart_read(uart_t uart);

#endif // DRIVER_UART_H