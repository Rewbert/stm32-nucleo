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

/**
 * @brief Write a NULL-terminated string to the specified UART.
 * 
 */
void uart_write_string(uart_t uart, char *str);

#endif // DRIVER_UART_H